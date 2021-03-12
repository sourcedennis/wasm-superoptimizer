// criterion.rs may be useful for benchmarking. criterion.rs is mainly made to
// benchmark Rust programs, though. TODO: investigate? The benchmarks (in
// `main.rs`) work pretty good, though



// use wasmer_runtime::{error, imports, instantiate, Func};
// use wasmer_runtime::types::{WasmExternType};
// use wasmer_runtime_core::typed_func::{Wasm, WasmTypeList};
// use criterion::{BenchmarkId, criterion_group, criterion_main, Criterion};
// use rand::{Rng, SeedableRng};
// use rand::rngs::{StdRng};
// use std::time::Duration;

// // Benchmarks functions inside WebAssembly modules. It is setup for
// // micro-benchmarks of modules that expose a `runall` function.
// // See the `criterion_benchmark(..)` function at the bottom.

// // This must be a macro, as `$slow_file` and `$fast_file` must be constant strings (by `include_bytes!(..)`)
// macro_rules! benchmark_pair {
//   ($name:expr, $input:tt, $output:tt, $slow_file:expr, $fast_file:expr, $gen:expr, $n:expr, $c:expr) => {{
//     // Define function so `?` syntactic sugar can be used
//     let mut f = || -> error::Result< () > {
//       let mut group = $c.benchmark_group( $name );

//       let wasm_bytes_slow = include_bytes!( $slow_file );
//       let wasm_bytes_opt  = include_bytes!( $fast_file );
//       let import_object   = imports! {};
    
//       let instance_slow = instantiate( wasm_bytes_slow, &import_object )?;
//       let instance_opt  = instantiate( wasm_bytes_opt,  &import_object )?;
    
//       let func_run_slow: Func< $input, $output > = instance_slow.exports.get( "runall" )?;
//       let func_run_opt: Func< $input, $output >  = instance_opt.exports.get( "runall" )?;

//       for _i in 0..$n {
//         let x = $gen( );
    
//         group.bench_with_input(
//             BenchmarkId::new( "slow", format!( "{:?}", x ) )
//           , &x
//           , |b, i| b.iter( || func_run_slow.tcall( *i ) )
//           );
//         group.bench_with_input(
//             BenchmarkId::new( "fast", format!( "{:?}", x ) )
//           , &x
//           , |b, i| b.iter( || func_run_opt.tcall( *i ) )
//           );
//       }

//       group.finish( );
    
//       Ok( () )
//     };

//     if f( ).is_err( ) {
//       println!( "Failed: {}", $name );
//     }
//   }}
// }

// // A helper trait which resolves the variable argument count for the `Func::call(..)` struct.
// trait TupleCallable< A, Rets: WasmTypeList > {
//   fn tcall( &self, a: A ) -> Result< Rets, error::RuntimeError >;
// }

// impl< 'a, Rets: WasmTypeList > TupleCallable< (), Rets > for Func< 'a, (), Rets, Wasm > {
//   fn tcall( &self, _a: () ) -> Result< Rets, error::RuntimeError > {
//     self.call( )
//   }
// }

// impl< 'a, A: WasmExternType, Rets: WasmTypeList > TupleCallable< (A,), Rets > for Func< 'a, A, Rets, Wasm > {
//   fn tcall( &self, (a,): (A,) ) -> Result< Rets, error::RuntimeError > {
//     self.call( a )
//   }
// }

// impl< 'a, A: WasmExternType, B: WasmExternType, Rets: WasmTypeList > TupleCallable< (A, B), Rets > for Func< 'a, (A, B), Rets, Wasm > {
//   fn tcall( &self, (a, b): (A, B) ) -> Result< Rets, error::RuntimeError > {
//     self.call( a, b )
//   }
// }

// impl< 'a, A: WasmExternType, B: WasmExternType, C: WasmExternType, Rets: WasmTypeList > TupleCallable< (A, B, C), Rets > for Func< 'a, (A, B, C), Rets, Wasm > {
//   fn tcall( &self, (a, b, c): (A, B, C) ) -> Result< Rets, error::RuntimeError > {
//     self.call( a, b, c )
//   }
// }

// fn new_rng( ) -> StdRng {
//   // Seed not at all carefully chosen. Should work for this case, though.
//   StdRng::seed_from_u64( 0x6D7F39AB19FC345E )
// }

// pub fn criterion_benchmark(c: &mut Criterion) {
//   let n = 10;

//   let mut rng: StdRng = new_rng( );
//   benchmark_pair!(
//     "popcount", u32, u32,
//     "../../../data/micro/popcount.wasm",
//     "../../../data/micro/popcount.opt.wasm",
//     || rng.gen( ), n, c );
  
//   rng = new_rng( );
//   benchmark_pair!(
//     "polynomial", u32, u32,
//     "../../../data/micro/polynomial.wasm",
//     "../../../data/micro/polynomial.opt.wasm",
//     || rng.gen( ), n, c );
  
//   rng = new_rng( );
//   benchmark_pair!(
//     "uselessloop", u32, u32,
//     "../../../data/micro/uselessloop.wasm",
//     "../../../data/micro/uselessloop.opt.wasm",
//     || rng.gen( ), n, c );
  
//   rng = new_rng( );
//   benchmark_pair!(
//     "loopfusion", (), (),
//     "../../../data/micro/loopfusion.wasm",
//     "../../../data/micro/loopfusion.opt.wasm",
//     || rng.gen( ), 1, c );
  
//   rng = new_rng( );
//   benchmark_pair!(
//     "leqand", u32, u32,
//     "../../../data/micro/leqand.wasm",
//     "../../../data/micro/leqand.opt.wasm",
//     || rng.gen( ), n, c );
  
//   rng = new_rng( );
//   benchmark_pair!(
//     "transitive", (u32, u32, u32), u32,
//     "../../../data/micro/transitive.wasm",
//     "../../../data/micro/transitive.opt.wasm",
//     || rng.gen( ), n, c );
  
//   rng = new_rng( );
//   benchmark_pair!(
//     "eqbranches1", u32, u32,
//     "../../../data/micro/eqbranches1.wasm",
//     "../../../data/micro/eqbranches1.opt.wasm",
//     || rng.gen( ), n, c );
  
//   rng = new_rng( );
//   benchmark_pair!(
//     "eqbranches2", u32, u32,
//     "../../../data/micro/eqbranches2.wasm",
//     "../../../data/micro/eqbranches2.opt.wasm",
//     || rng.gen( ), n, c );
    
//   rng = new_rng( );
//   benchmark_pair!(
//     "swapif", (u32,u32), u32,
//     "../../../data/micro/swapif.wasm",
//     "../../../data/micro/swapif.opt.wasm",
//     || rng.gen( ), n, c );
    
//   rng = new_rng( );
//   benchmark_pair!(
//     "loopsplit", (), (),
//     "../../../data/micro/loopsplit.wasm",
//     "../../../data/micro/loopsplit.opt.wasm",
//     || rng.gen( ), 1, c );
// }

// criterion_group!{
//   name    = benches;
//   config  =
//       Criterion::default( )
//         .warm_up_time( Duration::from_secs( 5 ) )
//         .measurement_time( Duration::from_secs( 15 ) );
//   targets = criterion_benchmark
// }
// criterion_main!(benches);
