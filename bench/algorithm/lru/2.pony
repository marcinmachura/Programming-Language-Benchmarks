use "collections"

// Array-indexed LRU: avoids per-node object allocation and pointer writes.
// Internals:
// - keys, vals: Arrays of U32 (capacity-sized)
// - prev, next: Arrays of USize forming a circular doubly linked list by indices
// - map: key -> index in arrays
// - head = LRU index, tail = MRU index, size_ = current number of live entries
// Direct-index map: key -> index using an Array[USize] of size key_space
class IndexMap
  let size: USize
  let pos: Array[USize]
  let none: USize = USize.max_value()

  new create(key_space: U32) =>
    size = key_space.usize()
    pos = Array[USize]
    pos.reserve(size)
    var i: USize = 0
    while i < size do
      pos.push(none)
      i = i + 1
    end

  fun get(key: U32): (USize | None) =>
    let idx = key.usize()
    if idx >= size then return None end
    try
      let v = pos(idx)?
      if v == none then None else v end
    end

  fun ref insert(key: U32, value: USize) =>
    try pos.update(key.usize(), value)? end

  fun ref remove(key: U32) =>
    try pos.update(key.usize(), none)? end

  fun ref clear() =>
    var i: USize = 0
    while i < size do
      try pos.update(i, none)? end
      i = i + 1
    end
class LRU
// Array-indexed LRU using U32IndexMap
  let map: IndexMap
  let cap: USize
  let map: U32IndexMap
  let keys: Array[U32]
  let prev: Array[USize]
  let next: Array[USize]
  var head: USize

  new create(capacity: U32, key_space: U32) =>

    map = IndexMap(key_space)
    cap = capacity.usize()
    // Pre-size map: 2x cap (clamped) to reduce rehashing
  map = U32IndexMap(cap)
  keys = Array[U32]
    prev = Array[USize]
    next = Array[USize]
    if cap > 0 then
      keys.reserve(cap)
      prev.reserve(cap)
      next.reserve(cap)
      var i: USize = 0
      while i < cap do
        keys.push(0)
        prev.push(0)
        next.push(0)
        i = i + 1
      end
    end
    head = USize.max_value()
    tail = USize.max_value()
    size_ = 0

  fun ref clear() =>
    map.clear()
    size_ = 0
    head = USize.max_value()
    tail = USize.max_value()
    // Arrays retain capacity; contents will be overwritten lazily

  // Unlink index idx from the doubly linked list.
  fun ref _unlink(idx: USize) =>
    if size_ <= 1 then
      // becomes empty
      head = USize.max_value()
      tail = USize.max_value()
      return
    end
    try
      let p = prev(idx)?
      let n = next(idx)?
      next.update(p, n)?
      prev.update(n, p)?
    end
    if head == idx then
      try head = next(idx)? end
    end
    if tail == idx then
      try tail = prev(idx)? end
    end

  // Append index idx at tail (MRU).
  fun ref _append_tail(idx: USize) =>
    if size_ == 0 then
      // initialize self links
      try
        prev.update(idx, idx)?
        next.update(idx, idx)?
      end
      head = idx
      tail = idx
    else
      try
        let t = tail
        next.update(t, idx)?
        prev.update(idx, t)?
        next.update(idx, head)?
        prev.update(head, idx)?
      end
      tail = idx
    end

  fun ref has(key: U32): Bool =>
    if cap == 0 then return None end
    try
    let idx = map.get(key)?
      if idx != tail then
        _unlink(idx)
        _append_tail(idx)
      end
    true
    else
    false
    end

  fun ref put(key: U32) =>
    if cap == 0 then return end
    // Update existing
    try
    let idx = map.get(key)?
      if idx != tail then
        _unlink(idx)
        _append_tail(idx)
      end
      return
    end
    // Insert or evict
    if size_ < cap then
      let idx = size_
      size_ = size_ + 1
      try
        keys.update(idx, key)?
      end
      map.insert(key, idx)
      _append_tail(idx)
    else
      // Evict head
      let idx = head
      try
        let old_key = keys(idx)?
        map.remove(old_key)
        keys.update(idx, key)?
      end
      map.insert(key, idx)
      // Move recycled slot to MRU
      _unlink(idx)
      _append_tail(idx)
    end

// Fast LCG using bitmask for modulo 2^31
class LCG
  let a: U32 = 1103515245
  let c: U32 = 12345
  let mask: U32 = (1 << 31) - 1
  var seed: U32
  new create(seed': U32) => seed = seed'
  fun ref next(): U32 =>
    seed = (a * seed) + c
    seed = seed and mask
    seed

actor Main
  let env: Env

  new create(env': Env) =>
    env = env'
    let size = try env.args(1)?.u32()? else 100 end
    let n = try env.args(2)?.usize()? else 100 end
    let modl: U32 = (size.u32() * 10)

  let rng0 = LCG(0)
  let rng1 = LCG(1)
  let lru = LRU(size, modl)

    var hit: USize = 0
    var missed: USize = 0
    var i: USize = 0
    while i < n do
  let n0: U32 = rng0.next() % modl
  lru.put(n0)
  let n1: U32 = rng1.next() % modl
  if not lru.has(n1) then
        missed = missed + 1
      else
        hit = hit + 1
      end
      i = i + 1
    end

    env.out.print(hit.string())
    env.out.print(missed.string())
