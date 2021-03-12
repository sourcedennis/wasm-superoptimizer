use wasmer_runtime::{error, imports, instantiate, Func};
use wasmer_runtime::types::{WasmExternType};
use wasmer_runtime_core::typed_func::{Wasm, WasmTypeList};
use rand::{Rng, SeedableRng};
use rand::rngs::{StdRng};
use std::time::{Duration, Instant};
use csv::Writer;

// This must be a macro, as `$file` must be a constant strings (by `include_bytes!(..)`)
macro_rules! benchmark {
  ($input:tt, $output:tt, $file:expr, $vals:expr) => {{
    // Define function so `?` syntactic sugar can be used
    let f = || -> error::Result< Vec< u128 > > {
      let mut times = Vec::with_capacity( $vals.len( ) );

      let wasm_bytes    = include_bytes!( $file );
      let import_object = imports! {};
    
      let instance = instantiate( wasm_bytes, &import_object )?;
    
      let func_run: Func< $input, $output > = instance.exports.get( "runall" )?;

      for v in $vals {
        let start = Instant::now();
        for _j in 0..100 {
          if func_run.tcall( *v ).is_err( ) {
            panic!( "WASM error" );
          }
        }
        let duration = start.elapsed( );
        times.push( duration.as_nanos( ) );
      }
    
      Ok( times )
    };

    f( ).map_err( |_| "WASM error" )
  }}
}

// A helper trait which resolves the variable argument count for the `Func::call(..)` struct.
trait TupleCallable< A, Rets: WasmTypeList > {
  fn tcall( &self, a: A ) -> Result< Rets, error::RuntimeError >;
}

impl< 'a, Rets: WasmTypeList > TupleCallable< (), Rets > for Func< 'a, (), Rets, Wasm > {
  fn tcall( &self, _a: () ) -> Result< Rets, error::RuntimeError > {
    self.call( )
  }
}

impl< 'a, A: WasmExternType, Rets: WasmTypeList > TupleCallable< (A,), Rets > for Func< 'a, A, Rets, Wasm > {
  fn tcall( &self, (a,): (A,) ) -> Result< Rets, error::RuntimeError > {
    self.call( a )
  }
}

impl< 'a, A: WasmExternType, B: WasmExternType, Rets: WasmTypeList > TupleCallable< (A, B), Rets > for Func< 'a, (A, B), Rets, Wasm > {
  fn tcall( &self, (a, b): (A, B) ) -> Result< Rets, error::RuntimeError > {
    self.call( a, b )
  }
}

impl< 'a, A: WasmExternType, B: WasmExternType, C: WasmExternType, Rets: WasmTypeList > TupleCallable< (A, B, C), Rets > for Func< 'a, (A, B, C), Rets, Wasm > {
  fn tcall( &self, (a, b, c): (A, B, C) ) -> Result< Rets, error::RuntimeError > {
    self.call( a, b, c )
  }
}

impl< 'a, A: WasmExternType, B: WasmExternType, C: WasmExternType, D: WasmExternType, Rets: WasmTypeList > TupleCallable< (A, B, C, D), Rets > for Func< 'a, (A, B, C, D), Rets, Wasm > {
  fn tcall( &self, (a, b, c, d): (A, B, C, D) ) -> Result< Rets, error::RuntimeError > {
    self.call( a, b, c, d )
  }
}

fn new_rng( ) -> StdRng {
  // Seed not at all carefully chosen. Should work for this case, though.
  StdRng::seed_from_u64( 0x6D7F39AB19FC345E )
}

fn write_record< 'a, F: std::io::Write >( wtr: &mut Writer< F >, name: &str, times: &[u128] ) -> Result< (), &'a str > {
  let record = Some( name.to_string( ) ).into_iter( ).chain( times.into_iter( ).map( |x| x.to_string( ) ) );
  wtr.write_record( record ).map_err( |_| "CSV write error" )
}

fn gen_vals< T >( rng: &mut StdRng, n: usize ) -> Vec< T >
  where
    rand::distributions::Standard: rand::distributions::Distribution<T> {

  let mut vals = Vec::with_capacity( n );
  for _i in 0..n {
    vals.push( rng.gen( ) );
  }
  vals
}

fn main_res( ) -> Result< (), &'static str > {
  let n = 1000;

  let mut wtr = Writer::from_path("benchmarks_native.csv").map_err( |_| "CSV open error" )?;
  let mut rng: StdRng = new_rng( );
  
  // # idgcd

  // println!( "idgcd" );
  // let vals = gen_vals::< (u32,) >( &mut rng, n );

  // let res1 = benchmark!(
  //   u32, u32, // input, output
  //   "../../../data/micro/idgcd.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "idgcd", &res1 )?;

  // let res2 = benchmark!(
  //   u32, u32, // input, output
  //   "../../../data/micro/idgcd.opt.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "idgcd.opt", &res2 )?;
  
  // # bubblesort4

  let vals = gen_vals::< (u32,u32,u32,u32) >( &mut rng, n );

  println!( "bubblesort4" );
  let res1 = benchmark!(
    (u32,u32,u32,u32), (), // input, output
    "../../../data/micro/bubblesort4.exp.n=1m.wasm",
    &vals )?;
  write_record( &mut wtr, "bubblesort4", &res1 )?;

  println!( "bubblesort4.opt" );
  let res2 = benchmark!(
    (u32,u32,u32,u32), (), // input, output
    "../../../data/micro/bubblesort4.opt.exp.n=1m.wasm",
    &vals )?;
  write_record( &mut wtr, "bubblesort4.opt", &res2 )?;

  // # popcount

  // let vals = gen_vals::< (u32,) >( &mut rng, n );

  // println!( "popcount" );
  // let res1 = benchmark!(
  //   u32, u32, // input, output
  //   "../../../data/micro/popcount.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "popcount", &res1 )?;

  // println!( "popcount.opt" );
  // let res2 = benchmark!(
  //   u32, u32, // input, output
  //   "../../../data/micro/popcount.opt.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "popcount.opt", &res2 )?;
  
  // // # babbage

  // let vals = gen_vals::< () >( &mut rng, n );

  // println!( "babbage" );
  // let res1 = benchmark!(
  //   (), u32, // input, output
  //   "../../../data/micro/babbage.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "babbage", &res1 )?;

  // println!( "babbage.opt" );
  // let res2 = benchmark!(
  //   (), u32, // input, output
  //   "../../../data/micro/babbage.opt.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "babbage.opt", &res2 )?;
  
  // // # transitive_break

  // let vals = gen_vals::< (u32,u32,u32) >( &mut rng, n );

  // println!( "transitive" );
  // let res1 = benchmark!(
  //   (u32,u32,u32), u32, // input, output
  //   "../../../data/micro/transitive_break.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "transitive", &res1 )?;

  // println!( "transitive.opt" );
  // let res2 = benchmark!(
  //   (u32,u32,u32), u32, // input, output
  //   "../../../data/micro/transitive_break.opt.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "transitive.opt", &res2 )?;
  
  // // # two_ifs
  // // Doesn't work. wasmer does not seem to support multi-value

  // let vals = gen_vals::< (u32,) >( &mut rng, n );

  // println!( "two_ifs" );
  // let res1 = benchmark!(
  //   u32, u32, // input, output
  //   "../../../data/micro/two_ifs.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "two_ifs", &res1 )?;

  // println!( "two_ifs.opt" );
  // let res2 = benchmark!(
  //   u32, u32, // input, output
  //   "../../../data/micro/two_ifs.opt.exp.wasm",
  //   &vals )?;
  // write_record( &mut wtr, "two_ifs.opt", &res2 )?;
  
  // // # eqbranches1

  // let val = rng.gen( );

  // let res1 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/eqbranches1.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "eqbranches1", &res1 )?;

  // let res2 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/eqbranches1.opt.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "eqbranches1.opt", &res2 )?;
  
  // // # eqbranches2

  // let val = rng.gen( );

  // let res1 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/eqbranches2.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "eqbranches2", &res1 )?;

  // let res2 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/eqbranches2.opt.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "eqbranches2.opt", &res2 )?;
  
  // // # polynomial

  // let val = rng.gen( );

  // let res1 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/polynomial.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "polynomial", &res1 )?;

  // let res2 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/polynomial.opt.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "polynomial.opt", &res2 )?;
  
  // // # uselessloop

  // let val = rng.gen( );

  // let res1 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/uselessloop.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "uselessloop", &res1 )?;

  // let res2 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/uselessloop.opt.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "uselessloop.opt", &res2 )?;
  
  // // # loopfusion

  // let res1 = benchmark!(
  //   (), (),
  //   "../../../data/micro/loopfusion.wasm",
  //   (), n )?;
  // write_record( &mut wtr, "loopfusion", &res1 )?;

  // let res2 = benchmark!(
  //   (), (),
  //   "../../../data/micro/loopfusion.opt.wasm",
  //   (), n )?;
  // write_record( &mut wtr, "loopfusion.opt", &res2 )?;

  // // # leqand

  // let val = rng.gen( );

  // let res1 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/leqand.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "leqand", &res1 )?;

  // let res2 = benchmark!(
  //   u32, u32,
  //   "../../../data/micro/leqand.opt.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "leqand.opt", &res2 )?;
  
  // // # transitive

  // let val = rng.gen( );

  // let res1 = benchmark!(
  //   (u32, u32, u32), u32,
  //   "../../../data/micro/transitive.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "transitive", &res1 )?;

  // let res2 = benchmark!(
  //   (u32, u32, u32), u32,
  //   "../../../data/micro/transitive.opt.wasm",
  //   val, n )?;
  // write_record( &mut wtr, "transitive.opt", &res2 )?;
  
  wtr.flush( ).map_err( |_| "CSV close error" )?;

  Ok( () )
}

fn main( ) {
  if let Err( err ) = main_res( ) {
    println!( "Error: {:?}", err );
  } else {
    println!( "Done" )
  }
}
