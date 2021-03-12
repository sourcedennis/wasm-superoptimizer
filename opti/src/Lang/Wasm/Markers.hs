
-- | Type markers for some non-standard WebAssembly types.
module Lang.Wasm.Markers where


-- | Marker for a memory block
data TMem

-- | Indexes memory. A 32-bit integer is added to a 32-bit constant offset to
-- produce the memory index. This thus requires 33 bits.
data TI33
