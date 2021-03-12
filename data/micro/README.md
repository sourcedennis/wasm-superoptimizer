# micro

Small hand-crafted WebAssembly modules. Superoptimized versions can easily be inspected manually to determine improvements.

Most source programs are implemented in Rust. However, some are given in C (e.g., to represent low-level unsafe pointer transformations which are trickier in Rust).

## polynomial
[WebAssembly Studio implementation](https://webassembly.studio/?f=2m7lu498jr3)

The polynomial `(a+1)**4` is expanded to `a*a*a*a + 4*a*a*a + 6*a*a + 4*a + 1`. From this large formula, the optimal program likely is:
```rust
a = ( a + 1 );
a = a * a;
return a * a;
```

## leqand
[WebAssembly Studio implementation](https://webassembly.studio/?f=grjwn21xp7)
This always returns `a`:
```rust
if a < 0x80 {
  return a & 0xFF;
} else {
  return a;
}
```

## transitive
[WebAssembly Studio implementation](https://webassembly.studio/?f=d1r56x84zs)
Infeasible branches waste cycles. This always returns `1`:
```rust
if a > b {
  if b > c {
    if a <= c {
      return 2; // This can never happen
    }
  }
}
1
```
Note that this compiles to a nested `select` (non-branching) instruction.

## popcount
[WebAssembly Studio implementation](https://webassembly.studio/?f=7a63w663dji)
Performs a population count, which counts the number of bits set to `1` in a 32-bit word.
```rust
fn popcount( mut x: u32 ) -> u32 {
  let mut count = 0;
  while x != 0 {
    if ( x & 1 ) != 0 {
      count += 1;
    }
    x >>= 1;
  }
  count
}
```
WebAssembly has a built-in `i32.popcnt` instruction.

## conststreq
[WebAssembly Studio implementation](https://webassembly.studio/?f=ud7krut8hi)
Checks equivalence to a constant string:
```c
bool streq( char* a, char* b ) {
  while ( *a != 0 && *b != 0 ) {
    if ( *a != *b ) {
      return 0;
    }
    a++;
    b++;
  }
  return *a == *b;
}

uint32_t run( uint32_t ptr ) {
  return streq( (char *) ptr, "a" );
}
```
This compiles to a loop. A trivial implementation would be:
```c
uint32_t run( uint32_t ptr ) {
  char* input = (char *) ptr;
  return *input == 'a' && *(input+1) == 0;
}
```

## eqbranches1
[WebAssembly Studio implementation](https://webassembly.studio/?f=v7o3nvelatg)
The taken branch does not affect the result, as it always returns `x+1`:
```rust
fn run( x: u32 ) -> u32 {
  let mut a = 1;
  let mut b = 1;

  if x > 3 {
    a = x;
  } else {
    b = x;
  }

  a + b
}
```

## loopfusion
[WebAssembly Studio implementation](https://webassembly.studio/?f=iiwvw2obny)

Bounded loop function with bit-fiddle. In Rust:
```rust
// Assume the host writes to this memory
static mut DATA: [u32; 8] = [0; 8];

fn run( ) {
  unsafe {
    mul2( &mut DATA );
    add1( &mut DATA );
  }
}

fn mul2( data: &mut [u32] ) {
  for i in 0..data.len( ) {
    data[ i ] *= 2;
  }
}

fn add1( data: &mut [u32] ) {
  for i in 0..data.len( ) {
    data[ i ] += 1;
  }
}
```
Obviously, both `mul2` and `add1` independently loop over the data. These loops can be combined. From their combination a (bit-fiddle) optimization can be made; namely:
```rust
data[ i ] = ( data[ i ] * 2 ) + 1;
```
that expression can be replaced by:
```rust
data[ i ] = ( data[ i ] << 1 ) | 1;
```
Note that the left-shift introduces a right-most `0`; Thus `(x << 1) + 1 == (x << 1) | 1`, for any `x`.

The final function is thus (in C):
```c
void run( ) {
  uint32_t *ptr = DATA;

  *ptr = ( (*ptr) << 1 ) | 1;

  while ( ptr < 1024 + 7 * sizeof( uint32_t ) ) {
    ptr++;
    *ptr = ( (*ptr) << 1 ) | 1;
  }
}
```
Though, the C-compiler compiles this strangely also. A hand-written WebAssembly implementation of the above function can be found [here](https://webassembly.studio/?f=30dlb58pfx3).
Note that the truly optimal function would have the entire loop unrolled; That is a size-performance trade-off.

## eqbranches2
[WebAssembly Studio implementation](https://webassembly.studio/?f=g36jau7genq)

Both branches produce the same result; this is non-obvious.
```rust
if x <= 0x7FFFFFFF {
  min( 2*x, x )
} else {
  x
}
```

## uselessloop
[WebAssembly Studio implementation](https://webassembly.studio/?f=rly646tid6p)
The entire loop can be eliminated, but this is non-obvious to the compiler.
```rust
// Produces ( x+1, (x+1)*2 )
fn foo(x: u32) -> (u32,u32) {
  let mut a = 1;
  let mut b = 2;
  for i in 0..x {
    a += 1;
    b += 2;
  }
  ( a, b )
}

#[no_mangle]
fn run( x : u32 ) -> u32 {
  let (a,b) = foo( x );
  a + b // (x+1)*3
}
```

## swapif
This function returns `5` whenever `y < 4`. If the conditions were swapped, the `x < 2` in that branch can be eliminated.
```rust
fn run( x : u32, y : u32 ) -> u32 {
  if x < 2 {
    if y < 4 {
      5
    } else {
      8
    }
  } else {
    if y < 4 {
      5
    } else {
      6
    }
  }
}
```
Rust optimizes this to:
```rust
fn foo( x: u32, y: u32 ) {
  let c = ( y < 4 );
  
  let p = if c { 5 } else { 8 };
  let q = if c { 5 } else { 6 };
  
  if x < 2 { p } else { q }
}
```
It recognises the common expression in the condition, but does not attempt to unify the paths.

## loopsplit
[WebAssembly Studio implementation](https://webassembly.studio/?f=ma65zrihm1j)
The loop body performs tasks that only depend on the loop counter, so it can be split into two loops.
```rust
for i in 0..8 {
  if i < 4 {
	DATA[ i ] += 3;
  } else if i < 100 {
	DATA[ i ] += 9;
  } else {
	DATA[ i ] += 5;
  }
}
```
