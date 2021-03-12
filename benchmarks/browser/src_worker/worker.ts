declare function postMessage( msg : any ): void;

// TODO: Cleanup overlapping script instances

interface WasmScript {
  readonly granularity : number,
  readonly low : number,
  readonly high : number,
  /** Run `n` iterations on the webassembly. Returns the time in milliseconds
   *  taken to run all iterations.
   *  Note that it is responsible for measuring the time itself, as it may
   *  require some additional time to setup the session which should not be
   *  included in the measurement.
   */
  benchmark( instance: WebAssembly.Instance, n: number ): number
}

/**
 * Executes a single WASM function that takes two inputs and produces one
 * (e.g. min, max, gcd)
 */
class WasmScriptNullary implements WasmScript {
  /** Number of times the benchmark is repeated */
  public readonly granularity: number;

  public readonly low  : number;
  public readonly high : number;

  public constructor( low: number, high: number, granularity: number ) {
    // These values make no sense for the nullary case. TODO
    this.granularity = granularity;
    this.low  = low;
    this.high = high;
  }

  public benchmark( instance: WebAssembly.Instance, n : number ): number {
    // ## RUN ##
    let start = Date.now( );
    for ( let i = 0; i < n; i++ ) {
      (<any> instance.exports).runall( );
    }
    return Date.now( ) - start;
  }
}

/**
 * Executes a single WASM function that takes two inputs and produces one
 * (e.g. min, max, gcd)
 */
class WasmScriptUnary implements WasmScript {
  /** Number of times the benchmark is repeated */
  public readonly granularity: number;

  public readonly low  : number;
  public readonly high : number;

  public constructor( low: number, high: number, granularity: number ) {
    this.granularity = granularity;
    this.low = low;
    this.high = high;
  }

  public benchmark( instance: WebAssembly.Instance, n : number ): number {
    // ## SETUP ##
    let input = new Array( n );

    for ( let i = 0; i < n; i++ ) {
      input[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
    }

    // ## RUN ##
    let start = Date.now( );
    for ( let i = 0; i < n; i++ ) {
      (<any> instance.exports).runall( input[ i ] );
    }
    return Date.now( ) - start;
  }
}

/**
 * Executes a single WASM function that takes two inputs and produces one
 * (e.g. min, max, gcd)
 */
class WasmScriptBinary implements WasmScript {
  /** Number of times the benchmark is repeated */
  public readonly granularity: number;

  public readonly low  : number;
  public readonly high : number;

  public constructor( low: number, high: number, granularity: number ) {
    this.granularity = granularity;
    this.low  = low;
    this.high = high;
  }

  public benchmark( instance: WebAssembly.Instance, n : number ): number {
    // ## SETUP ##
    let input1 = new Array( n );
    let input2 = new Array( n );

    for ( let i = 0; i < n; i++ ) {
      input1[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
      input2[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
    }

    // ## RUN ##
    let start = Date.now( );
    for ( let i = 0; i < n; i++ ) {
      (<any> instance.exports).runall( input1[ i ], input2[ i ] );
    }
    return Date.now( ) - start;
  }
}

/**
 * Executes a single WASM function that takes three inputs and produces one
 * (e.g. min, max, gcd)
 */
class WasmScriptTernary implements WasmScript {
  /** Number of times the benchmark is repeated */
  public readonly granularity: number;

  public readonly low  : number;
  public readonly high : number;

  public constructor( low: number, high: number, granularity: number ) {
    this.granularity = granularity;
    this.low  = low;
    this.high = high;
  }

  public benchmark( instance: WebAssembly.Instance, n : number ): number {
    // ## SETUP ##
    let input1 = new Array( n );
    let input2 = new Array( n );
    let input3 = new Array( n );

    for ( let i = 0; i < n; i++ ) {
      input1[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
      input2[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
      input3[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
    }

    // ## RUN ##
    let start = Date.now( );
    for ( let i = 0; i < n; i++ ) {
      (<any> instance.exports).runall( input1[ i ], input2[ i ], input3[ i ] );
    }
    return Date.now( ) - start;
  }
}

/**
 * Executes a single WASM function that takes four inputs and produces one
 */
class WasmScriptQuaternary implements WasmScript {
  /** Number of times the benchmark is repeated */
  public readonly granularity: number;

  public readonly low  : number;
  public readonly high : number;

  public constructor( low: number, high: number, granularity: number ) {
    this.granularity = granularity;
    this.low  = low;
    this.high = high;
  }

  public benchmark( instance: WebAssembly.Instance, n : number ): number {
    // ## SETUP ##
    let input1 = new Array( n );
    let input2 = new Array( n );
    let input3 = new Array( n );
    let input4 = new Array( n );

    for ( let i = 0; i < n; i++ ) {
      input1[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
      input2[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
      input3[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
      input4[ i ] = Math.floor( Math.random( ) * ( this.high - this.low ) ) + this.low;
    }

    // ## RUN ##
    let start = Date.now( );
    for ( let i = 0; i < n; i++ ) {
      (<any> instance.exports).runall( input1[ i ], input2[ i ], input3[ i ], input4[ i ] );
    }
    return Date.now( ) - start;
  }
}

// ## Incoming Messages ##
interface MsgInit {
  readonly id: 'init';
  readonly module : WebAssembly.Module;
  /** Number of times the benchmark is repeated */
  readonly granularity : number;
  readonly low : number;
  readonly high : number;
  readonly type : 'nullary'|'unary'|'binary'|'ternary'|'quaternary';
}

interface MsgRun {
  readonly id: 'run';
}

// ## Outgoing Messages ##
interface MsgInitDone {
  readonly id: 'init-done';
  readonly index : number;
  readonly isOk: boolean;
}

interface MsgStepDone {
  readonly id: 'step-done',
  readonly index : number,
  readonly timeInPs : number,
  readonly pct : number
}

interface MsgRunDone {
  readonly id: 'run-done'
}

// ## Global State ##
const scripts : WasmScript[] = [];
const modules : WebAssembly.Module[] = [];
const instances : (WebAssembly.Instance | null)[] = [];

function handleMsgInit( msg : MsgInit ) {
  let index = modules.length;

  switch ( msg.type ) {
  case 'nullary':
    scripts.push( new WasmScriptNullary( msg.low, msg.high, msg.granularity ) );
    break;
  case 'unary':
    scripts.push( new WasmScriptUnary( msg.low, msg.high, msg.granularity ) );
    break;
  case 'binary':
    scripts.push( new WasmScriptBinary( msg.low, msg.high, msg.granularity ) );
    break;
  case 'ternary':
    scripts.push( new WasmScriptTernary( msg.low, msg.high, msg.granularity ) );
    break;
  case 'quaternary':
    scripts.push( new WasmScriptQuaternary( msg.low, msg.high, msg.granularity ) );
    break;
  default:
    console.error( 'Invalid arity' );
  }

  modules.push( msg.module );
  instances.push( null );

  WebAssembly.instantiate( msg.module, { } )
    .then( instance => {
      instances[ index ] = instance;
      postMessage( <MsgInitDone> { id: 'init-done', index, isOk: true } );
    } )
    .catch( ( ) => {
      postMessage( <MsgInitDone> { id: 'init-done', index, isOk: false } );
    } );
}

function handleMsgRun( msg: MsgRun ) {
  let indices: number[] = [];

  for ( let i = 0; i < instances.length; i++ ) {
    if ( instances[ i ] != null ) {
      for ( let j = 0; j < 1000; j++ ) {
        indices.push( i );
      }
    }
  }
  shuffle( indices );
  console.log( `#indices = ${indices.length}` );

  let numDone = 0;
  f( ).then( ( ) => {
    postMessage( <MsgRunDone> { id: 'run-done' } );
  } );

  function f( ): Promise< void > {
    if ( numDone < indices.length ) {
      let i = indices[ numDone ];
      console.log( 'running', i );
      
      let n = scripts[ i ].granularity;
      let timeMs = scripts[ i ].benchmark( <WebAssembly.Instance> instances[ i ], n );
      let timeInPs = ( timeMs * 1_000_000_000 ) / n;
      numDone++;
      postMessage( <MsgStepDone> { id: 'step-done', index: i, timeInPs, pct: numDone / indices.length } );
      return timeout( 1 ).then( ( ) => f( ) );
    } else {
      return Promise.resolve( );
    }
  }
}

function shuffle< T >( arr: T[] ) {
  for ( let i = 0; i < arr.length; i++ ) {
    let newI = Math.floor( Math.random( ) * arr.length );
    let tmp = arr[ i ];
    arr[ i ] = arr[ newI ];
    arr[ newI ] = tmp;
  }
}

function timeout( delayMs: number ): Promise< void > {
  return new Promise( ( fResolve, _fReject ) => {
    setTimeout( ( ) => fResolve( ), delayMs );
  } );
}

onmessage = ev => {
  let msg = ev.data;

  switch ( msg.id ) {
  case 'init': handleMsgInit( msg ); break;
  case 'run': handleMsgRun( msg ); break;
  }
}
