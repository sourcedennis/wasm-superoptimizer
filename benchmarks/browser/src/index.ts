import IdgcdWasm    from '@microwasm/idgcd.exp.wasm';
import IdgcdOptWasm from '@microwasm/idgcd.opt.exp.wasm';
import BubblesortWasm    from '@microwasm/bubblesort4.exp.wasm';
import BubblesortOptWasm from '@microwasm/bubblesort4.opt.exp.wasm';
import PopcountWasm    from '@microwasm/popcount.exp.wasm';
import PopcountOptWasm from '@microwasm/popcount.opt.exp.wasm';
import BabbageWasm    from '@microwasm/babbage.exp.wasm';
import BabbageOptWasm from '@microwasm/babbage.opt.exp.wasm';
import TransitiveWasm    from '@microwasm/transitive_break.exp.wasm';
import TransitiveOptWasm from '@microwasm/transitive_break.opt.exp.wasm';
import TwoIfsWasm    from '@microwasm/two_ifs.exp.wasm';
import TwoIfsOptWasm from '@microwasm/two_ifs.opt.exp.wasm';

class WasmScript {
  public readonly name : string;
  /** Link to inspect it externally (e.g. in webassembly.studio) */
  public readonly link : string;
  public readonly location : string;
  public readonly granularity : number;
  public readonly low : number;
  public readonly high : number;
  public readonly type : 'nullary'|'unary'|'binary'|'ternary'|'quaternary';

  public constructor( name : string, link : string, location: string, granularity : number, low : number, high : number, type: 'nullary'|'unary'|'binary'|'ternary'|'quaternary' ) {
    this.name = name;
    this.link = link;
    this.location = location;
    this.granularity = granularity;
    this.low = low;
    this.high = high;
    this.type = type;
  }
}

document.addEventListener( 'DOMContentLoaded', ev => {
  console.log( PopcountWasm );


  let div = document.createElement( 'div' );
  document.body.appendChild( div );

  const granularity = 1000;

  let scripts =
    [ new WasmScript( 'idgcd', '#', IdgcdWasm, granularity, 0, 0xFFFFFFFF, 'unary' )
    , new WasmScript( 'idgcd.opt', '#', IdgcdOptWasm, granularity, 0, 0xFFFFFFFF, 'unary' )

    , new WasmScript( 'bubblesort4', '#', BubblesortWasm, granularity, 0, 0xFFFFFFFF, 'quaternary' )
    , new WasmScript( 'bubblesort4.opt', '#', BubblesortOptWasm, granularity, 0, 0xFFFFFFFF, 'quaternary' )

    , new WasmScript( 'popcount', '#', PopcountWasm, granularity, 0, 0xFFFFFFFF, 'unary' )
    , new WasmScript( 'popcount.opt', '#', PopcountOptWasm, granularity, 0, 0xFFFFFFFF, 'unary' )

    , new WasmScript( 'babbage', '#', BabbageWasm, granularity / 10, 0, 0xFFFFFFFF, 'nullary' )
    , new WasmScript( 'babbage.opt', '#', BabbageOptWasm, granularity * 10000, 0, 0xFFFFFFFF, 'nullary' )

    , new WasmScript( 'transitive', '#', TransitiveWasm, granularity, 0, 0xFFFFFFFF, 'ternary' )
    , new WasmScript( 'transitive.opt', '#', TransitiveOptWasm, granularity, 0, 0xFFFFFFFF, 'ternary' )

    , new WasmScript( 'two_ifs', '#', TwoIfsWasm, granularity, 0, 0xFFFFFFFF, 'unary' )
    , new WasmScript( 'two_ifs.opt', '#', TwoIfsOptWasm, granularity, 0, 0xFFFFFFFF, 'unary' )
    ];
  
  console.log( scripts ); 

  let worker = new Worker( 'worker.js' );
  let allModules: Promise<WebAssembly.Module>[] = [];
  let measurements: number[][] = [];

  for ( let i = 0; i < scripts.length; i++ ) {
    let script = scripts[ i ];

    let p = WebAssembly.compileStreaming( fetch( script.location ) );
    allModules.push( p );
    measurements.push( [] );
  }

  Promise.all( allModules ).then( modules => {
    for ( let i = 0; i < scripts.length; i++ ) {
      let script = scripts[ i ];
      worker.postMessage( { id: 'init', module: modules[ i ], granularity: script.granularity, low: script.low, high: script.high, type: script.type } );
    }
  } );

  let numInitDone = 0;

  worker.addEventListener( 'message', ev => {
    let msg = ev.data;
    
    console.log( msg );

    switch ( msg.id ) {
    case 'init-done': {
      numInitDone++;

      if ( numInitDone == scripts.length ) {
        worker.postMessage( { id: 'run' } );
      }
      break;
    }
    case 'step-done': {
      measurements[ msg.index ].push( msg.timeInPs );
      break;
    }
    case 'run-done': {
      console.log( 'run done!' );

      let csvLines: string[] = [];

      for ( let i = 0; i < scripts.length; i++ ) {
        let line = [ scripts[ i ].name ];
        line.push( ...measurements[ i ].map( x => x.toString( ) ) );
        csvLines.push( line.join( ',' ) );
      }

      console.log( csvLines.join( '\n' ) );

      break;
    }
    }
  } );
} );

/** Returns 95% confidence interval over the measurements */
function confAvg( measurements: number[] ): [number, number] {
  let n = measurements.length;
  let sum = 0;

  for ( let m of measurements ) {
    sum += m;
  }

  let avg = sum / n;

  let variance = 0;

  for ( let m of measurements ) {
    variance += ( 1 / n ) * ( m - avg ) * ( m - avg );
  }

  let sd = Math.sqrt( variance );
  let dev = 1.960 * sd / Math.sqrt( n ); // 95% confidence deviation

  let avgRound = Math.round( avg );
  let devRound = Math.ceil( Math.max( Math.abs( avg + dev - avgRound ), Math.abs( avg - dev - avgRound ) ) );

  return [ avgRound, devRound ];
}
