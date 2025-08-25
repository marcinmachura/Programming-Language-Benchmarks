use "collections"

// multi-threaded implementation (Go/Zig based)

actor Main
  new create(env: Env) =>
    let n: USize = try env.args(1)?.usize()? else 12 end
    let coordinator = Coordinator(env, n)
    coordinator.start()

class val Result
  let max_flips: USize
  let check_sum: ISize
  
  new create(mf: USize, cs: ISize) =>
    max_flips = mf
    check_sum = cs

actor Coordinator
  let _env: Env
  let _n: USize
  let _workers: USize = 4
  var _results_received: USize = 0
  var _max_flips: USize = 0
  var _check_sum: ISize = 0
  let _fact: Array[USize] val
  let _chunks: USize = 720
  var _chunk_size: USize = 0
  var _tasks: USize = 0
  
  new create(env: Env, n: USize) =>
    _env = env
    _n = n
    _fact = recover val
      var a = Array[USize](_n + 1)
      var prev: USize = 1
      a.push(prev)
      for i in Range[USize](1, _n + 1) do
        prev = prev * i
        a.push(prev)
      end
      a
    end
    
    try
      let factn = _fact(_n)?
      let t1 = factn + _chunks
      let t2 = t1 - 1
      _chunk_size = t2 / _chunks
      _chunk_size = _chunk_size + (_chunk_size % 2)
      let t3 = factn + _chunk_size
      let t4 = t3 - 1
      _tasks = t4 / _chunk_size
    end
  
  be start() =>
    for i in Range[USize](0, _tasks) do
      let worker = Worker(this, _n, _fact, i * _chunk_size, _chunk_size)
      worker.compute()
    end
  
  be result(r: Result val) =>
    _results_received = _results_received + 1
    if r.max_flips > _max_flips then
      _max_flips = r.max_flips
    end
    _check_sum = _check_sum + r.check_sum
    
    if _results_received == _tasks then
      _env.out.print(_check_sum.string())
      _env.out.print("Pfannkuchen(" + _n.string() + ") = " + _max_flips.string())
    end

actor Worker
  let _coordinator: Coordinator
  let _n: USize
  let _fact: Array[USize] val
  let _idx_min: USize
  let _chunk_size: USize
  
  new create(coordinator: Coordinator, n: USize, fact: Array[USize] val, 
             idx_min: USize, chunk_size: USize) =>
    _coordinator = coordinator
    _n = n
    _fact = fact
    _idx_min = idx_min
    _chunk_size = chunk_size
  
  be compute() =>
    try
      var idx_max = _idx_min + _chunk_size
      if idx_max > _fact(_n)? then
        idx_max = _fact(_n)?
      end
      
      let p = Array[USize](_n)
      let pp = Array[USize](_n)
      let count = Array[USize](_n)
      
      // Initialize arrays
      for i in Range[USize](0, _n) do
        p.push(i)
        pp.push(0)
        count.push(0)
      end
      
      // Generate first permutation
      var idx = _idx_min
      var i = _n - 1
      while i > 0 do
        let d = idx / _fact(i)?
        count(i)? = d
        idx = idx % _fact(i)?
        
        // Copy p to pp
        for j in Range[USize](0, _n) do
          pp(j)? = p(j)?
        end
        
        // Rotate
        for j in Range[USize](0, i + 1) do
          if (j + d) <= i then
            p(j)? = pp(j + d)?
          else
            p(j)? = pp((j + d) - i - 1)?
          end
        end
        i = i - 1
      end
      
      var max_flips: USize = 1
      var check_sum: ISize = 0
      var sign = true
      idx = _idx_min
      
      while idx < idx_max do
        // Count flips
        let first = p(0)?
        if first != 0 then
          var flips: USize = 1
          if p(first)? != 0 then
            // Copy p to pp
              for ii in Range[USize](0, _n) do
                pp(ii)? = p(ii)?
              end
            
            var p0 = first
            while pp(p0)? != 0 do
              flips = flips + 1
              // Reverse
                var ii: USize = 1
                var j = p0 - 1
                while ii < j do
                  let tmp = pp(ii)?
                  pp(ii)? = pp(j)?
                  pp(j)? = tmp
                  ii = ii + 1
                  j = j - 1
                end
              let t = pp(p0)?
              pp(p0)? = p0
              p0 = t
            end
          end
          
          if flips > max_flips then
            max_flips = flips
          end
          
          if sign then
            check_sum = check_sum + flips.isize()
          else
            check_sum = check_sum - flips.isize()
          end
        end
        
        idx = idx + 1
        if idx == idx_max then break end
        
        // Generate next permutation
        if sign then
          let tmp = p(0)?
          p(0)? = p(1)?
          p(1)? = tmp
        else
          let tmp = p(1)?
          p(1)? = p(2)?
          p(2)? = tmp
          
          var k: USize = 2
          while true do
            let c = count(k)? + 1
            count(k)? = c
            if c <= k then break end

            count(k)? = 0
            // rotate left p[0..k]
            let p0 = p(0)?
            for j in Range[USize](0, k) do
              p(j)? = p(j + 1)?
            end
            p(k)? = p0
            k = k + 1
          end
        end
        sign = not sign
      end
      
      _coordinator.result(Result(max_flips, check_sum))
    end
