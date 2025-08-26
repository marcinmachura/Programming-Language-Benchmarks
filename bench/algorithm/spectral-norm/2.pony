primitive SN
  // Compute v = A * u using O(1) incremental updates for the denominator.
  fun a_times_u(u: Array[F64] box, v: Array[F64] ref) ? =>
    let n = u.size()
    var i: USize = 0
    while i < n do
      var sum: F64 = 0
      var den: USize = ((i * (i + 1)) >> 1) + i + 1
      var inc: USize = i + 1
      var denf: F64 = den.f64()
      var incf: F64 = inc.f64()
      var j: USize = 0
      while j < n do
        sum = sum + (u(j)? / denf)
        denf = denf + incf
        incf = incf + 1.0
        j = j + 1
      end
      v.update(i, sum)?
      i = i + 1
    end

  // Compute v = A^T * u with incremental denominator updates.
  fun at_times_u(u: Array[F64] box, v: Array[F64] ref) ? =>
    let n = u.size()
    var i: USize = 0
    while i < n do
      var sum: F64 = 0
      var den: USize = ((i * (i + 1)) >> 1) + 1
      var inc: USize = i + 2
      var denf: F64 = den.f64()
      var incf: F64 = inc.f64()
      var j: USize = 0
      while j < n do
        sum = sum + (u(j)? / denf)
        denf = denf + incf
        incf = incf + 1.0
        j = j + 1
      end
      v.update(i, sum)?
      i = i + 1
    end

  fun ata_times_u(out: Array[F64] ref, inp: Array[F64] box, scratch: Array[F64] ref) ? =>
    a_times_u(inp, scratch)?
    at_times_u(scratch, out)?

  fun fmt9(x: F64): String iso^ =>
    var i: I64 = x.trunc().i64()
    let frac: F64 = (x - i.f64()).abs()
    var f: I64 = (frac * 1_000_000_000.0).round().i64()
    if f == 1_000_000_000 then
      f = 0
      if x >= 0 then
        i = i + 1
      else
        i = i - 1
      end
    end
    let s = recover iso String end
    let istr = i.string()
    s.append(consume istr)
    s.push(46)
    let fs = f.string()
    let pad: USize = if fs.size() < 9 then 9 - fs.size() else 0 end
    var k: USize = 0
    while k < pad do s.push(48); k = k + 1 end
    s.append(consume fs)
    consume s

actor Main
  new create(env: Env) =>
    let n: USize = try env.args(1)?.usize()? else 100 end
    let workers: USize = if n < 4 then n else 4 end
    if workers <= 1 then
      _run_seq(env, n)
    else
      let mgr = Manager(env, n, workers)
      mgr.start()
    end

  fun _run_seq(env: Env, n: USize) =>
    let iters: USize = 10
    let u = Array[F64].init(1.0, n)
    let v = Array[F64].init(0.0, n)
    let x = Array[F64].init(0.0, n)
    var i: USize = 0
    while i < iters do
      try SN.ata_times_u(v, u, x)? end
      try SN.ata_times_u(u, v, x)? end
      i = i + 1
    end
    var vBv: F64 = 0
    var vv: F64 = 0
    var k: USize = 0
    try
      while k < n do
        vBv = vBv + (u(k)? * v(k)?)
        vv = vv + (v(k)? * v(k)?)
        k = k + 1
      end
    end
  let res = (vBv / vv).sqrt()
  let s = SN.fmt9(res)
    env.out.print(consume s)

actor Manager
  let _env: Env
  let _n: USize
  let _workers: Array[Worker tag] ref
  let _parts: Array[(USize, USize)] ref
  var _u: Array[F64] ref
  var _v: Array[F64] ref
  var _tmp: Array[F64] ref
  var _iters: USize = 10
  var _pending: USize = 0
  var _stage: U8 = 0
  var _vBv_acc: F64 = 0
  var _vv_acc: F64 = 0

  new create(env: Env, n: USize, nworkers: USize) =>
    _env = env
    _n = n
    _workers = Array[Worker tag]
    _parts = Array[(USize, USize)]
    _u = Array[F64].init(1.0, n)
    _v = Array[F64].init(0.0, n)
    _tmp = Array[F64].init(0.0, n)
    let k = if nworkers > 0 then nworkers else 1 end
    var base: USize = 0
    var r: USize = 0
    let chunk: USize = n / k
    let rem: USize = n % k
    var i: USize = 0
    while i < k do
      r = if i < rem then 1 else 0 end
      let s: USize = base
      let e: USize = s + chunk + r
      if s < e then _parts.push((s, e)) end
      base = e
      i = i + 1
    end
    i = 0
    while i < _parts.size() do
      _workers.push(Worker)
      i = i + 1
    end

  be start() =>
    _stage = 0
    try _kickoff()? end

  fun ref _kickoff() ? =>
    match _stage
    | 0 => _broadcast_av(_u)?
    | 1 => _broadcast_atv(_tmp, true)?
    | 2 => _broadcast_av(_v)?
    | 3 => _broadcast_atv(_tmp, false)?
    else None
    end

  fun ref _broadcast_av(src: Array[F64] box) ? =>
    let tmp = recover iso Array[F64] end
    tmp.reserve(_n)
    var ci: USize = 0
    try
      while ci < _n do
        tmp.push(src(ci)?)
        ci = ci + 1
      end
    end
    let imm: Array[F64] val = consume tmp
    _pending = _workers.size()
    var i: USize = 0
    while i < _workers.size() do
      let pe = _parts(i)?
      let s = pe._1
      let e = pe._2
  _workers(i)?.compute_av(s, e, imm, _n, this)
      i = i + 1
    end

  fun ref _broadcast_atv(src: Array[F64] box, into_v: Bool) ? =>
    let tmp = recover iso Array[F64] end
    tmp.reserve(_n)
    var ci: USize = 0
    try
      while ci < _n do
        tmp.push(src(ci)?)
        ci = ci + 1
      end
    end
    let imm: Array[F64] val = consume tmp
    _pending = _workers.size()
    var i: USize = 0
    while i < _workers.size() do
      let pe = _parts(i)?
      let s = pe._1
      let e = pe._2
  _workers(i)?.compute_atv(s, e, imm, _n, this, into_v)
      i = i + 1
    end

  be _on_av_segment(offset: USize, seg: Array[F64] iso) =>
    let a = consume seg
    try
      var j: USize = 0
      let m = a.size()
      while j < m do
        _tmp.update(offset + j, a(j)?)?
        j = j + 1
      end
    end
    _pending = _pending - 1
    if _pending == 0 then
      _stage = _stage + 1
      try _kickoff()? end
    end

  be _on_atv_segment(offset: USize, seg: Array[F64] iso, into_v: Bool) =>
    let a = consume seg
    try
      var j: USize = 0
      let m = a.size()
      if into_v then
        while j < m do
          _v.update(offset + j, a(j)?)?
          j = j + 1
        end
      else
        while j < m do
          _u.update(offset + j, a(j)?)?
          j = j + 1
        end
      end
    end
    _pending = _pending - 1
    if _pending == 0 then
      if _stage == 1 then
        _stage = 2
        try _kickoff()? end
      elseif _stage == 3 then
        if _iters > 1 then
          _iters = _iters - 1
          _stage = 0
          try _kickoff()? end
        else
          try _broadcast_dot()? end
        end
      end
    end

  fun ref _broadcast_dot() ? =>
    _pending = _workers.size()
    _vBv_acc = 0
    _vv_acc = 0
    // Snapshot u and v as immutable for send
    let u_iso = recover iso Array[F64] end
    u_iso.reserve(_n)
    let v_iso = recover iso Array[F64] end
    v_iso.reserve(_n)
    var di: USize = 0
    try
      while di < _n do
        u_iso.push(_u(di)?)
        v_iso.push(_v(di)?)
        di = di + 1
      end
    end
    let u_val: Array[F64] val = consume u_iso
    let v_val: Array[F64] val = consume v_iso
    var i: USize = 0
    while i < _workers.size() do
      let pe = _parts(i)?
      let s = pe._1
      let e = pe._2
      _workers(i)?.compute_dot(s, e, u_val, v_val, this)
      i = i + 1
    end

  be _on_dot_result(vbv: F64, vv: F64) =>
    _vBv_acc = _vBv_acc + vbv
    _vv_acc = _vv_acc + vv
    _pending = _pending - 1
    if _pending == 0 then
  let res = (_vBv_acc / _vv_acc).sqrt()
  let s = SN.fmt9(res)
      _env.out.print(consume s)
    end

actor Worker
  be compute_av(start: USize, stopAt: USize, u: Array[F64] val, n: USize, reply: Manager tag) =>
    let len = stopAt - start
    let seg = recover iso Array[F64] end
    seg.reserve(len)
    var i: USize = start
    while i < stopAt do
      var sum: F64 = 0
      var den: USize = ((i * (i + 1)) >> 1) + i + 1
      var inc: USize = i + 1
      var denf: F64 = den.f64()
      var incf: F64 = inc.f64()
      var j: USize = 0
      try
        while j < n do
          sum = sum + (u(j)? / denf)
          denf = denf + incf
          incf = incf + 1.0
          j = j + 1
        end
      end
      seg.push(sum)
      i = i + 1
    end
    reply._on_av_segment(start, consume seg)

  be compute_atv(start: USize, stopAt: USize, inp: Array[F64] val, n: USize, reply: Manager tag, into_v: Bool) =>
    let len = stopAt - start
    let seg = recover iso Array[F64] end
    seg.reserve(len)
    var i: USize = start
    while i < stopAt do
      var sum: F64 = 0
      var den: USize = ((i * (i + 1)) >> 1) + 1
      var inc: USize = i + 2
      var denf: F64 = den.f64()
      var incf: F64 = inc.f64()
      var j: USize = 0
      try
        while j < n do
          sum = sum + (inp(j)? / denf)
          denf = denf + incf
          incf = incf + 1.0
          j = j + 1
        end
      end
      seg.push(sum)
      i = i + 1
    end
    reply._on_atv_segment(start, consume seg, into_v)

  be compute_dot(start: USize, stopAt: USize, u: Array[F64] val, v: Array[F64] val, reply: Manager tag) =>
    var vbv: F64 = 0
    var vv: F64 = 0
    var i: USize = start
    try
      while i < stopAt do
        vbv = vbv + (u(i)? * v(i)?)
        vv = vv + (v(i)? * v(i)?)
        i = i + 1
      end
    end
    reply._on_dot_result(vbv, vv)

  // Compute v = A * u using O(1) incremental updates for the denominator.
  fun _a_times_u(u: Array[F64] box, v: Array[F64] ref) ? =>
    let n = u.size()
    var i: USize = 0
    while i < n do
      var sum: F64 = 0
      // For fixed row i, denominator D starts at:
      // D0 = T(i) + i + 1, where T(x) = x*(x+1)/2.
      var den: USize = ((i * (i + 1)) >> 1) + i + 1
      // Each step j -> j+1, D increases by (i + j + 1).
      var inc: USize = i + 1  // when j = 0, increment = i + 1
      var denf: F64 = den.f64()
      var incf: F64 = inc.f64()
      var j: USize = 0
      while j < n do
        sum = sum + (u(j)? / denf)
        denf = denf + incf
        incf = incf + 1.0
        j = j + 1
      end
      v.update(i, sum)?
      i = i + 1
    end

  // Compute v = A^T * u with incremental denominator updates.
  fun _at_times_u(u: Array[F64] box, v: Array[F64] ref) ? =>
    let n = u.size()
    var i: USize = 0
    while i < n do
      var sum: F64 = 0
      // For column i of A (row i of A^T), denominator is:
      // D(j) = T(j + i) + j + 1. With j -> j+1, D increases by (j + i + 2).
      var den: USize = ((i * (i + 1)) >> 1) + 1  // when j = 0: T(i) + 0 + 1
      var inc: USize = i + 2                     // starts at (0 + i + 2)
      var denf: F64 = den.f64()
      var incf: F64 = inc.f64()
      var j: USize = 0
      while j < n do
        sum = sum + (u(j)? / denf)
        denf = denf + incf
        incf = incf + 1.0
        j = j + 1
      end
      v.update(i, sum)?
      i = i + 1
    end

  fun _ata_times_u(out: Array[F64] ref, inp: Array[F64] box, scratch: Array[F64] ref) ? =>
    // scratch and out are length n
    _a_times_u(inp, scratch)?
    _at_times_u(scratch, out)?

  fun _fmt9(x: F64): String iso^ =>
    var i: I64 = x.trunc().i64()
    let frac: F64 = (x - i.f64()).abs()
    var f: I64 = (frac * 1_000_000_000.0).round().i64()
    if f == 1_000_000_000 then
      f = 0
      if x >= 0 then
        i = i + 1
      else
        i = i - 1
      end
    end
    let s = recover iso String end
    let istr = i.string()
    s.append(consume istr)
    s.push(46)
    let fs = f.string()
    let pad: USize = if fs.size() < 9 then 9 - fs.size() else 0 end
    var k: USize = 0
    while k < pad do s.push(48); k = k + 1 end
    s.append(consume fs)
    consume s