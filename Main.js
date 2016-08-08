"use strict";
// This object will hold all exports.
var Haste = {};
if(typeof window === 'undefined') window = global;

/* Constructor functions for small ADTs. */
function T0(t){this._=t;}
function T1(t,a){this._=t;this.a=a;}
function T2(t,a,b){this._=t;this.a=a;this.b=b;}
function T3(t,a,b,c){this._=t;this.a=a;this.b=b;this.c=c;}
function T4(t,a,b,c,d){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;}
function T5(t,a,b,c,d,e){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;}
function T6(t,a,b,c,d,e,f){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;this.f=f;}

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// "Zero" object; used to avoid creating a whole bunch of new objects
// in the extremely common case of a nil-like data constructor.
var __Z = new T0(0);

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

// Indicates that a closure-creating tail loop isn't done.
var __continue = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof Function) {
            if(args.length === f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else if(f instanceof PAP) {
            if(args.length === f.arity) {
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                return new PAP(f.f, f.args.concat(args));
            } else {
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else {
            return f;
        }
    }
}

function A1(f, x) {
    f = E(f);
    if(f instanceof Function) {
        return f.length === 1 ? f(x) : new PAP(f, [x]);
    } else if(f instanceof PAP) {
        return f.arity === 1 ? f.f.apply(null, f.args.concat([x]))
                             : new PAP(f.f, f.args.concat([x]));
    } else {
        return f;
    }
}

function A2(f, x, y) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 2:  return f(x, y);
        case 1:  return A1(B(f(x)), y);
        default: return new PAP(f, [x,y]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 2:  return f.f.apply(null, f.args.concat([x,y]));
        case 1:  return A1(B(f.f.apply(null, f.args.concat([x]))), y);
        default: return new PAP(f.f, f.args.concat([x,y]));
        }
    } else {
        return f;
    }
}

function A3(f, x, y, z) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 3:  return f(x, y, z);
        case 2:  return A1(B(f(x, y)), z);
        case 1:  return A2(B(f(x)), y, z);
        default: return new PAP(f, [x,y,z]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 3:  return f.f.apply(null, f.args.concat([x,y,z]));
        case 2:  return A1(B(f.f.apply(null, f.args.concat([x,y]))), z);
        case 1:  return A2(B(f.f.apply(null, f.args.concat([x]))), y, z);
        default: return new PAP(f.f, f.args.concat([x,y,z]));
        }
    } else {
        return f;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            if(t.x === __updatable) {
                var f = t.f;
                t.f = __blackhole;
                t.x = f();
            } else {
                return t.f();
            }
        }
        if(t.x === __updatable) {
            throw 'Infinite loop!';
        } else {
            return t.x;
        }
    } else {
        return t;
    }
}

/* Tail call chain counter. */
var C = 0, Cs = [];

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    Cs.push(C);
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        C = 0;
        f = fun();
    }
    C = Cs.pop();
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return {_:0, a:(a-a%b)/b, b:a%b};
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return {_:0, a:x & 0xffffffff, b:x > 0x7fffffff};
}

function subC(a, b) {
    var x = a-b;
    return {_:0, a:x & 0xffffffff, b:x < -2147483648};
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, __Z);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return {_:1,a:str.charCodeAt(i),b:new T(function() {
            return unAppCStr(str,chrs,i+1);
        })};
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str._ == 1; str = E(str.b)) {
        s += String.fromCharCode(E(str.a));
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x.a;
    return x.b;
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
}

/* Convert a JS exception into a Haskell JSException */
function __hsException(e) {
  e = e.toString();
  var x = new Long(2904464383, 3929545892, true);
  var y = new Long(3027541338, 3270546716, true);
  var t = new T5(0, x, y
                  , new T5(0, x, y
                            , unCStr("haste-prim")
                            , unCStr("Haste.Prim.Foreign")
                            , unCStr("JSException")), __Z, __Z);
  var show = function(x) {return unCStr(E(x).a);}
  var dispEx = function(x) {return unCStr("JavaScript exception: " + E(x).a);}
  var showList = function(_, s) {return unAppCStr(e, s);}
  var showsPrec = function(_, _p, s) {return unAppCStr(e, s);}
  var showDict = new T3(0, showsPrec, show, showList);
  var self;
  var fromEx = function(_) {return new T1(1, self);}
  var dict = new T5(0, t, showDict, null /* toException */, fromEx, dispEx);
  self = new T2(0, dict, new T1(0, e));
  return self;
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        if(typeof e._ === 'undefined') {
            e = __hsException(e);
        }
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Object) {
        return x._;
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round, rintDouble = jsRound, rintFloat = jsRound;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt64(i) {
    return popCnt(i.low) + popCnt(i.high);
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function __clz(bits, x) {
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    } else {
        return bits - (1 + Math.floor(Math.log(x)/Math.LN2));
    }
}

// TODO: can probably be done much faster with arithmetic tricks like __clz
function __ctz(bits, x) {
    var y = 1;
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    }
    for(var i = 0; i < bits; ++i) {
        if(y & x) {
            return i;
        } else {
            y <<= 1;
        }
    }
    return 0;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    if(x === 0) {
        return __decodedZeroF;
    }
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return {_:0, a:sign*man, b:exp};
}

var __decodedZero = {_:0,a:1,b:0,c:0,d:0};
var __decodedZeroF = {_:0,a:1,b:0};

function decodeDouble(x) {
    if(x === 0) {
        // GHC 7.10+ *really* doesn't like 0 to be represented as anything
        // but zeroes all the way.
        return __decodedZero;
    }
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return {_:0, a:sign, b:manHigh, c:manLow, d:exp};
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs._) {
        strs = E(strs);
        arr.push(E(strs.a));
        strs = E(strs.b);
    }
    return arr.join(sep);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return __Z;
    }
    return {_:1,a:hs};
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return {_:0, a:jsRead(obj)};
    case 'string':
        return {_:1, a:obj};
    case 'boolean':
        return {_:2, a:obj}; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return {_:3, a:arr2lst_json(obj, 0)};
        } else if (obj == null) {
            return {_:5};
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = {_:1, a:{_:0, a:ks[i], b:toHS(obj[ks[i]])}, b:xs};
            }
            return {_:4, a:xs};
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1, a:toHS(arr[elem]), b:new T(function() {return arr2lst_json(arr,elem+1);}),c:true}
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

/* bn.js by Fedor Indutny, see doc/LICENSE.bn for license */
var __bn = {};
(function (module, exports) {
'use strict';

function BN(number, base, endian) {
  // May be `new BN(bn)` ?
  if (number !== null &&
      typeof number === 'object' &&
      Array.isArray(number.words)) {
    return number;
  }

  this.negative = 0;
  this.words = null;
  this.length = 0;

  if (base === 'le' || base === 'be') {
    endian = base;
    base = 10;
  }

  if (number !== null)
    this._init(number || 0, base || 10, endian || 'be');
}
if (typeof module === 'object')
  module.exports = BN;
else
  exports.BN = BN;

BN.BN = BN;
BN.wordSize = 26;

BN.max = function max(left, right) {
  if (left.cmp(right) > 0)
    return left;
  else
    return right;
};

BN.min = function min(left, right) {
  if (left.cmp(right) < 0)
    return left;
  else
    return right;
};

BN.prototype._init = function init(number, base, endian) {
  if (typeof number === 'number') {
    return this._initNumber(number, base, endian);
  } else if (typeof number === 'object') {
    return this._initArray(number, base, endian);
  }
  if (base === 'hex')
    base = 16;

  number = number.toString().replace(/\s+/g, '');
  var start = 0;
  if (number[0] === '-')
    start++;

  if (base === 16)
    this._parseHex(number, start);
  else
    this._parseBase(number, base, start);

  if (number[0] === '-')
    this.negative = 1;

  this.strip();

  if (endian !== 'le')
    return;

  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initNumber = function _initNumber(number, base, endian) {
  if (number < 0) {
    this.negative = 1;
    number = -number;
  }
  if (number < 0x4000000) {
    this.words = [ number & 0x3ffffff ];
    this.length = 1;
  } else if (number < 0x10000000000000) {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff
    ];
    this.length = 2;
  } else {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff,
      1
    ];
    this.length = 3;
  }

  if (endian !== 'le')
    return;

  // Reverse the bytes
  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initArray = function _initArray(number, base, endian) {
  if (number.length <= 0) {
    this.words = [ 0 ];
    this.length = 1;
    return this;
  }

  this.length = Math.ceil(number.length / 3);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  var off = 0;
  if (endian === 'be') {
    for (var i = number.length - 1, j = 0; i >= 0; i -= 3) {
      var w = number[i] | (number[i - 1] << 8) | (number[i - 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  } else if (endian === 'le') {
    for (var i = 0, j = 0; i < number.length; i += 3) {
      var w = number[i] | (number[i + 1] << 8) | (number[i + 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  }
  return this.strip();
};

function parseHex(str, start, end) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r <<= 4;

    // 'a' - 'f'
    if (c >= 49 && c <= 54)
      r |= c - 49 + 0xa;

    // 'A' - 'F'
    else if (c >= 17 && c <= 22)
      r |= c - 17 + 0xa;

    // '0' - '9'
    else
      r |= c & 0xf;
  }
  return r;
}

BN.prototype._parseHex = function _parseHex(number, start) {
  // Create possibly bigger array to ensure that it fits the number
  this.length = Math.ceil((number.length - start) / 6);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  // Scan 24-bit chunks and add them to the number
  var off = 0;
  for (var i = number.length - 6, j = 0; i >= start; i -= 6) {
    var w = parseHex(number, i, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
    off += 24;
    if (off >= 26) {
      off -= 26;
      j++;
    }
  }
  if (i + 6 !== start) {
    var w = parseHex(number, start, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
  }
  this.strip();
};

function parseBase(str, start, end, mul) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r *= mul;

    // 'a'
    if (c >= 49)
      r += c - 49 + 0xa;

    // 'A'
    else if (c >= 17)
      r += c - 17 + 0xa;

    // '0' - '9'
    else
      r += c;
  }
  return r;
}

BN.prototype._parseBase = function _parseBase(number, base, start) {
  // Initialize as zero
  this.words = [ 0 ];
  this.length = 1;

  // Find length of limb in base
  for (var limbLen = 0, limbPow = 1; limbPow <= 0x3ffffff; limbPow *= base)
    limbLen++;
  limbLen--;
  limbPow = (limbPow / base) | 0;

  var total = number.length - start;
  var mod = total % limbLen;
  var end = Math.min(total, total - mod) + start;

  var word = 0;
  for (var i = start; i < end; i += limbLen) {
    word = parseBase(number, i, i + limbLen, base);

    this.imuln(limbPow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }

  if (mod !== 0) {
    var pow = 1;
    var word = parseBase(number, i, number.length, base);

    for (var i = 0; i < mod; i++)
      pow *= base;
    this.imuln(pow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }
};

BN.prototype.copy = function copy(dest) {
  dest.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    dest.words[i] = this.words[i];
  dest.length = this.length;
  dest.negative = this.negative;
};

BN.prototype.clone = function clone() {
  var r = new BN(null);
  this.copy(r);
  return r;
};

// Remove leading `0` from `this`
BN.prototype.strip = function strip() {
  while (this.length > 1 && this.words[this.length - 1] === 0)
    this.length--;
  return this._normSign();
};

BN.prototype._normSign = function _normSign() {
  // -0 = 0
  if (this.length === 1 && this.words[0] === 0)
    this.negative = 0;
  return this;
};

var zeros = [
  '',
  '0',
  '00',
  '000',
  '0000',
  '00000',
  '000000',
  '0000000',
  '00000000',
  '000000000',
  '0000000000',
  '00000000000',
  '000000000000',
  '0000000000000',
  '00000000000000',
  '000000000000000',
  '0000000000000000',
  '00000000000000000',
  '000000000000000000',
  '0000000000000000000',
  '00000000000000000000',
  '000000000000000000000',
  '0000000000000000000000',
  '00000000000000000000000',
  '000000000000000000000000',
  '0000000000000000000000000'
];

var groupSizes = [
  0, 0,
  25, 16, 12, 11, 10, 9, 8,
  8, 7, 7, 7, 7, 6, 6,
  6, 6, 6, 6, 6, 5, 5,
  5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5, 5, 5, 5
];

var groupBases = [
  0, 0,
  33554432, 43046721, 16777216, 48828125, 60466176, 40353607, 16777216,
  43046721, 10000000, 19487171, 35831808, 62748517, 7529536, 11390625,
  16777216, 24137569, 34012224, 47045881, 64000000, 4084101, 5153632,
  6436343, 7962624, 9765625, 11881376, 14348907, 17210368, 20511149,
  24300000, 28629151, 33554432, 39135393, 45435424, 52521875, 60466176
];

BN.prototype.toString = function toString(base, padding) {
  base = base || 10;
  var padding = padding | 0 || 1;
  if (base === 16 || base === 'hex') {
    var out = '';
    var off = 0;
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var w = this.words[i];
      var word = (((w << off) | carry) & 0xffffff).toString(16);
      carry = (w >>> (24 - off)) & 0xffffff;
      if (carry !== 0 || i !== this.length - 1)
        out = zeros[6 - word.length] + word + out;
      else
        out = word + out;
      off += 2;
      if (off >= 26) {
        off -= 26;
        i--;
      }
    }
    if (carry !== 0)
      out = carry.toString(16) + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else if (base === (base | 0) && base >= 2 && base <= 36) {
    var groupSize = groupSizes[base];
    var groupBase = groupBases[base];
    var out = '';
    var c = this.clone();
    c.negative = 0;
    while (c.cmpn(0) !== 0) {
      var r = c.modn(groupBase).toString(base);
      c = c.idivn(groupBase);

      if (c.cmpn(0) !== 0)
        out = zeros[groupSize - r.length] + r + out;
      else
        out = r + out;
    }
    if (this.cmpn(0) === 0)
      out = '0' + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else {
    throw 'Base should be between 2 and 36';
  }
};

BN.prototype.toJSON = function toJSON() {
  return this.toString(16);
};

BN.prototype.toArray = function toArray(endian, length) {
  this.strip();
  var littleEndian = endian === 'le';
  var res = new Array(this.byteLength());
  res[0] = 0;

  var q = this.clone();
  if (!littleEndian) {
    // Assume big-endian
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[res.length - i - 1] = b;
    }
  } else {
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[i] = b;
    }
  }

  if (length) {
    while (res.length < length) {
      if (littleEndian)
        res.push(0);
      else
        res.unshift(0);
    }
  }

  return res;
};

if (Math.clz32) {
  BN.prototype._countBits = function _countBits(w) {
    return 32 - Math.clz32(w);
  };
} else {
  BN.prototype._countBits = function _countBits(w) {
    var t = w;
    var r = 0;
    if (t >= 0x1000) {
      r += 13;
      t >>>= 13;
    }
    if (t >= 0x40) {
      r += 7;
      t >>>= 7;
    }
    if (t >= 0x8) {
      r += 4;
      t >>>= 4;
    }
    if (t >= 0x02) {
      r += 2;
      t >>>= 2;
    }
    return r + t;
  };
}

// Return number of used bits in a BN
BN.prototype.bitLength = function bitLength() {
  var hi = 0;
  var w = this.words[this.length - 1];
  var hi = this._countBits(w);
  return (this.length - 1) * 26 + hi;
};

BN.prototype.byteLength = function byteLength() {
  return Math.ceil(this.bitLength() / 8);
};

// Return negative clone of `this`
BN.prototype.neg = function neg() {
  if (this.cmpn(0) === 0)
    return this.clone();

  var r = this.clone();
  r.negative = this.negative ^ 1;
  return r;
};

BN.prototype.ineg = function ineg() {
  this.negative ^= 1;
  return this;
};

// Or `num` with `this` in-place
BN.prototype.iuor = function iuor(num) {
  while (this.length < num.length)
    this.words[this.length++] = 0;

  for (var i = 0; i < num.length; i++)
    this.words[i] = this.words[i] | num.words[i];

  return this.strip();
};

BN.prototype.ior = function ior(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuor(num);
};


// Or `num` with `this`
BN.prototype.or = function or(num) {
  if (this.length > num.length)
    return this.clone().ior(num);
  else
    return num.clone().ior(this);
};

BN.prototype.uor = function uor(num) {
  if (this.length > num.length)
    return this.clone().iuor(num);
  else
    return num.clone().iuor(this);
};


// And `num` with `this` in-place
BN.prototype.iuand = function iuand(num) {
  // b = min-length(num, this)
  var b;
  if (this.length > num.length)
    b = num;
  else
    b = this;

  for (var i = 0; i < b.length; i++)
    this.words[i] = this.words[i] & num.words[i];

  this.length = b.length;

  return this.strip();
};

BN.prototype.iand = function iand(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuand(num);
};


// And `num` with `this`
BN.prototype.and = function and(num) {
  if (this.length > num.length)
    return this.clone().iand(num);
  else
    return num.clone().iand(this);
};

BN.prototype.uand = function uand(num) {
  if (this.length > num.length)
    return this.clone().iuand(num);
  else
    return num.clone().iuand(this);
};


// Xor `num` with `this` in-place
BN.prototype.iuxor = function iuxor(num) {
  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  for (var i = 0; i < b.length; i++)
    this.words[i] = a.words[i] ^ b.words[i];

  if (this !== a)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];

  this.length = a.length;

  return this.strip();
};

BN.prototype.ixor = function ixor(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuxor(num);
};


// Xor `num` with `this`
BN.prototype.xor = function xor(num) {
  if (this.length > num.length)
    return this.clone().ixor(num);
  else
    return num.clone().ixor(this);
};

BN.prototype.uxor = function uxor(num) {
  if (this.length > num.length)
    return this.clone().iuxor(num);
  else
    return num.clone().iuxor(this);
};


// Add `num` to `this` in-place
BN.prototype.iadd = function iadd(num) {
  // negative + positive
  if (this.negative !== 0 && num.negative === 0) {
    this.negative = 0;
    var r = this.isub(num);
    this.negative ^= 1;
    return this._normSign();

  // positive + negative
  } else if (this.negative === 0 && num.negative !== 0) {
    num.negative = 0;
    var r = this.isub(num);
    num.negative = 1;
    return r._normSign();
  }

  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) + (b.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }

  this.length = a.length;
  if (carry !== 0) {
    this.words[this.length] = carry;
    this.length++;
  // Copy the rest of the words
  } else if (a !== this) {
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  }

  return this;
};

// Add `num` to `this`
BN.prototype.add = function add(num) {
  if (num.negative !== 0 && this.negative === 0) {
    num.negative = 0;
    var res = this.sub(num);
    num.negative ^= 1;
    return res;
  } else if (num.negative === 0 && this.negative !== 0) {
    this.negative = 0;
    var res = num.sub(this);
    this.negative = 1;
    return res;
  }

  if (this.length > num.length)
    return this.clone().iadd(num);
  else
    return num.clone().iadd(this);
};

// Subtract `num` from `this` in-place
BN.prototype.isub = function isub(num) {
  // this - (-num) = this + num
  if (num.negative !== 0) {
    num.negative = 0;
    var r = this.iadd(num);
    num.negative = 1;
    return r._normSign();

  // -this - num = -(this + num)
  } else if (this.negative !== 0) {
    this.negative = 0;
    this.iadd(num);
    this.negative = 1;
    return this._normSign();
  }

  // At this point both numbers are positive
  var cmp = this.cmp(num);

  // Optimization - zeroify
  if (cmp === 0) {
    this.negative = 0;
    this.length = 1;
    this.words[0] = 0;
    return this;
  }

  // a > b
  var a;
  var b;
  if (cmp > 0) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) - (b.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }

  // Copy rest of the words
  if (carry === 0 && i < a.length && a !== this)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  this.length = Math.max(this.length, i);

  if (a !== this)
    this.negative = 1;

  return this.strip();
};

// Subtract `num` from `this`
BN.prototype.sub = function sub(num) {
  return this.clone().isub(num);
};

function smallMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  var len = (self.length + num.length) | 0;
  out.length = len;
  len = (len - 1) | 0;

  // Peel one iteration (compiler can't do it, because of code complexity)
  var a = self.words[0] | 0;
  var b = num.words[0] | 0;
  var r = a * b;

  var lo = r & 0x3ffffff;
  var carry = (r / 0x4000000) | 0;
  out.words[0] = lo;

  for (var k = 1; k < len; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = carry >>> 26;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = (k - j) | 0;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;
    }
    out.words[k] = rword | 0;
    carry = ncarry | 0;
  }
  if (carry !== 0) {
    out.words[k] = carry | 0;
  } else {
    out.length--;
  }

  return out.strip();
}

function bigMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  out.length = self.length + num.length;

  var carry = 0;
  var hncarry = 0;
  for (var k = 0; k < out.length - 1; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = hncarry;
    hncarry = 0;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;

      hncarry += ncarry >>> 26;
      ncarry &= 0x3ffffff;
    }
    out.words[k] = rword;
    carry = ncarry;
    ncarry = hncarry;
  }
  if (carry !== 0) {
    out.words[k] = carry;
  } else {
    out.length--;
  }

  return out.strip();
}

BN.prototype.mulTo = function mulTo(num, out) {
  var res;
  if (this.length + num.length < 63)
    res = smallMulTo(this, num, out);
  else
    res = bigMulTo(this, num, out);
  return res;
};

// Multiply `this` by `num`
BN.prototype.mul = function mul(num) {
  var out = new BN(null);
  out.words = new Array(this.length + num.length);
  return this.mulTo(num, out);
};

// In-place Multiplication
BN.prototype.imul = function imul(num) {
  if (this.cmpn(0) === 0 || num.cmpn(0) === 0) {
    this.words[0] = 0;
    this.length = 1;
    return this;
  }

  var tlen = this.length;
  var nlen = num.length;

  this.negative = num.negative ^ this.negative;
  this.length = this.length + num.length;
  this.words[this.length - 1] = 0;

  for (var k = this.length - 2; k >= 0; k--) {
    // Sum all words with the same `i + j = k` and accumulate `carry`,
    // note that carry could be >= 0x3ffffff
    var carry = 0;
    var rword = 0;
    var maxJ = Math.min(k, nlen - 1);
    for (var j = Math.max(0, k - tlen + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = this.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      carry += (r / 0x4000000) | 0;
      lo += rword;
      rword = lo & 0x3ffffff;
      carry += lo >>> 26;
    }
    this.words[k] = rword;
    this.words[k + 1] += carry;
    carry = 0;
  }

  // Propagate overflows
  var carry = 0;
  for (var i = 1; i < this.length; i++) {
    var w = (this.words[i] | 0) + carry;
    this.words[i] = w & 0x3ffffff;
    carry = w >>> 26;
  }

  return this.strip();
};

BN.prototype.imuln = function imuln(num) {
  // Carry
  var carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = (this.words[i] | 0) * num;
    var lo = (w & 0x3ffffff) + (carry & 0x3ffffff);
    carry >>= 26;
    carry += (w / 0x4000000) | 0;
    // NOTE: lo is 27bit maximum
    carry += lo >>> 26;
    this.words[i] = lo & 0x3ffffff;
  }

  if (carry !== 0) {
    this.words[i] = carry;
    this.length++;
  }

  return this;
};

BN.prototype.muln = function muln(num) {
  return this.clone().imuln(num);
};

// `this` * `this`
BN.prototype.sqr = function sqr() {
  return this.mul(this);
};

// `this` * `this` in-place
BN.prototype.isqr = function isqr() {
  return this.mul(this);
};

// Shift-left in-place
BN.prototype.iushln = function iushln(bits) {
  var r = bits % 26;
  var s = (bits - r) / 26;
  var carryMask = (0x3ffffff >>> (26 - r)) << (26 - r);

  if (r !== 0) {
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var newCarry = this.words[i] & carryMask;
      var c = ((this.words[i] | 0) - newCarry) << r;
      this.words[i] = c | carry;
      carry = newCarry >>> (26 - r);
    }
    if (carry) {
      this.words[i] = carry;
      this.length++;
    }
  }

  if (s !== 0) {
    for (var i = this.length - 1; i >= 0; i--)
      this.words[i + s] = this.words[i];
    for (var i = 0; i < s; i++)
      this.words[i] = 0;
    this.length += s;
  }

  return this.strip();
};

BN.prototype.ishln = function ishln(bits) {
  return this.iushln(bits);
};

// Shift-right in-place
BN.prototype.iushrn = function iushrn(bits, hint, extended) {
  var h;
  if (hint)
    h = (hint - (hint % 26)) / 26;
  else
    h = 0;

  var r = bits % 26;
  var s = Math.min((bits - r) / 26, this.length);
  var mask = 0x3ffffff ^ ((0x3ffffff >>> r) << r);
  var maskedWords = extended;

  h -= s;
  h = Math.max(0, h);

  // Extended mode, copy masked part
  if (maskedWords) {
    for (var i = 0; i < s; i++)
      maskedWords.words[i] = this.words[i];
    maskedWords.length = s;
  }

  if (s === 0) {
    // No-op, we should not move anything at all
  } else if (this.length > s) {
    this.length -= s;
    for (var i = 0; i < this.length; i++)
      this.words[i] = this.words[i + s];
  } else {
    this.words[0] = 0;
    this.length = 1;
  }

  var carry = 0;
  for (var i = this.length - 1; i >= 0 && (carry !== 0 || i >= h); i--) {
    var word = this.words[i] | 0;
    this.words[i] = (carry << (26 - r)) | (word >>> r);
    carry = word & mask;
  }

  // Push carried bits as a mask
  if (maskedWords && carry !== 0)
    maskedWords.words[maskedWords.length++] = carry;

  if (this.length === 0) {
    this.words[0] = 0;
    this.length = 1;
  }

  this.strip();

  return this;
};

BN.prototype.ishrn = function ishrn(bits, hint, extended) {
  return this.iushrn(bits, hint, extended);
};

// Shift-left
BN.prototype.shln = function shln(bits) {
  var x = this.clone();
  var neg = x.negative;
  x.negative = false;
  x.ishln(bits);
  x.negative = neg;
  return x;
};

BN.prototype.ushln = function ushln(bits) {
  return this.clone().iushln(bits);
};

// Shift-right
BN.prototype.shrn = function shrn(bits) {
  var x = this.clone();
  if(x.negative) {
      x.negative = false;
      x.ishrn(bits);
      x.negative = true;
      return x.isubn(1);
  } else {
      return x.ishrn(bits);
  }
};

BN.prototype.ushrn = function ushrn(bits) {
  return this.clone().iushrn(bits);
};

// Test if n bit is set
BN.prototype.testn = function testn(bit) {
  var r = bit % 26;
  var s = (bit - r) / 26;
  var q = 1 << r;

  // Fast case: bit is much higher than all existing words
  if (this.length <= s) {
    return false;
  }

  // Check bit and return
  var w = this.words[s];

  return !!(w & q);
};

// Add plain number `num` to `this`
BN.prototype.iaddn = function iaddn(num) {
  if (num < 0)
    return this.isubn(-num);

  // Possible sign change
  if (this.negative !== 0) {
    if (this.length === 1 && (this.words[0] | 0) < num) {
      this.words[0] = num - (this.words[0] | 0);
      this.negative = 0;
      return this;
    }

    this.negative = 0;
    this.isubn(num);
    this.negative = 1;
    return this;
  }

  // Add without checks
  return this._iaddn(num);
};

BN.prototype._iaddn = function _iaddn(num) {
  this.words[0] += num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] >= 0x4000000; i++) {
    this.words[i] -= 0x4000000;
    if (i === this.length - 1)
      this.words[i + 1] = 1;
    else
      this.words[i + 1]++;
  }
  this.length = Math.max(this.length, i + 1);

  return this;
};

// Subtract plain number `num` from `this`
BN.prototype.isubn = function isubn(num) {
  if (num < 0)
    return this.iaddn(-num);

  if (this.negative !== 0) {
    this.negative = 0;
    this.iaddn(num);
    this.negative = 1;
    return this;
  }

  this.words[0] -= num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] < 0; i++) {
    this.words[i] += 0x4000000;
    this.words[i + 1] -= 1;
  }

  return this.strip();
};

BN.prototype.addn = function addn(num) {
  return this.clone().iaddn(num);
};

BN.prototype.subn = function subn(num) {
  return this.clone().isubn(num);
};

BN.prototype.iabs = function iabs() {
  this.negative = 0;

  return this;
};

BN.prototype.abs = function abs() {
  return this.clone().iabs();
};

BN.prototype._ishlnsubmul = function _ishlnsubmul(num, mul, shift) {
  // Bigger storage is needed
  var len = num.length + shift;
  var i;
  if (this.words.length < len) {
    var t = new Array(len);
    for (var i = 0; i < this.length; i++)
      t[i] = this.words[i];
    this.words = t;
  } else {
    i = this.length;
  }

  // Zeroify rest
  this.length = Math.max(this.length, len);
  for (; i < this.length; i++)
    this.words[i] = 0;

  var carry = 0;
  for (var i = 0; i < num.length; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    var right = (num.words[i] | 0) * mul;
    w -= right & 0x3ffffff;
    carry = (w >> 26) - ((right / 0x4000000) | 0);
    this.words[i + shift] = w & 0x3ffffff;
  }
  for (; i < this.length - shift; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    carry = w >> 26;
    this.words[i + shift] = w & 0x3ffffff;
  }

  if (carry === 0)
    return this.strip();

  carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = -(this.words[i] | 0) + carry;
    carry = w >> 26;
    this.words[i] = w & 0x3ffffff;
  }
  this.negative = 1;

  return this.strip();
};

BN.prototype._wordDiv = function _wordDiv(num, mode) {
  var shift = this.length - num.length;

  var a = this.clone();
  var b = num;

  // Normalize
  var bhi = b.words[b.length - 1] | 0;
  var bhiBits = this._countBits(bhi);
  shift = 26 - bhiBits;
  if (shift !== 0) {
    b = b.ushln(shift);
    a.iushln(shift);
    bhi = b.words[b.length - 1] | 0;
  }

  // Initialize quotient
  var m = a.length - b.length;
  var q;

  if (mode !== 'mod') {
    q = new BN(null);
    q.length = m + 1;
    q.words = new Array(q.length);
    for (var i = 0; i < q.length; i++)
      q.words[i] = 0;
  }

  var diff = a.clone()._ishlnsubmul(b, 1, m);
  if (diff.negative === 0) {
    a = diff;
    if (q)
      q.words[m] = 1;
  }

  for (var j = m - 1; j >= 0; j--) {
    var qj = (a.words[b.length + j] | 0) * 0x4000000 +
             (a.words[b.length + j - 1] | 0);

    // NOTE: (qj / bhi) is (0x3ffffff * 0x4000000 + 0x3ffffff) / 0x2000000 max
    // (0x7ffffff)
    qj = Math.min((qj / bhi) | 0, 0x3ffffff);

    a._ishlnsubmul(b, qj, j);
    while (a.negative !== 0) {
      qj--;
      a.negative = 0;
      a._ishlnsubmul(b, 1, j);
      if (a.cmpn(0) !== 0)
        a.negative ^= 1;
    }
    if (q)
      q.words[j] = qj;
  }
  if (q)
    q.strip();
  a.strip();

  // Denormalize
  if (mode !== 'div' && shift !== 0)
    a.iushrn(shift);
  return { div: q ? q : null, mod: a };
};

BN.prototype.divmod = function divmod(num, mode, positive) {
  if (this.negative !== 0 && num.negative === 0) {
    var res = this.neg().divmod(num, mode);
    var div;
    var mod;
    if (mode !== 'mod')
      div = res.div.neg();
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.add(num);
    }
    return {
      div: div,
      mod: mod
    };
  } else if (this.negative === 0 && num.negative !== 0) {
    var res = this.divmod(num.neg(), mode);
    var div;
    if (mode !== 'mod')
      div = res.div.neg();
    return { div: div, mod: res.mod };
  } else if ((this.negative & num.negative) !== 0) {
    var res = this.neg().divmod(num.neg(), mode);
    var mod;
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.isub(num);
    }
    return {
      div: res.div,
      mod: mod
    };
  }

  // Both numbers are positive at this point

  // Strip both numbers to approximate shift value
  if (num.length > this.length || this.cmp(num) < 0)
    return { div: new BN(0), mod: this };

  // Very short reduction
  if (num.length === 1) {
    if (mode === 'div')
      return { div: this.divn(num.words[0]), mod: null };
    else if (mode === 'mod')
      return { div: null, mod: new BN(this.modn(num.words[0])) };
    return {
      div: this.divn(num.words[0]),
      mod: new BN(this.modn(num.words[0]))
    };
  }

  return this._wordDiv(num, mode);
};

// Find `this` / `num`
BN.prototype.div = function div(num) {
  return this.divmod(num, 'div', false).div;
};

// Find `this` % `num`
BN.prototype.mod = function mod(num) {
  return this.divmod(num, 'mod', false).mod;
};

BN.prototype.umod = function umod(num) {
  return this.divmod(num, 'mod', true).mod;
};

// Find Round(`this` / `num`)
BN.prototype.divRound = function divRound(num) {
  var dm = this.divmod(num);

  // Fast case - exact division
  if (dm.mod.cmpn(0) === 0)
    return dm.div;

  var mod = dm.div.negative !== 0 ? dm.mod.isub(num) : dm.mod;

  var half = num.ushrn(1);
  var r2 = num.andln(1);
  var cmp = mod.cmp(half);

  // Round down
  if (cmp < 0 || r2 === 1 && cmp === 0)
    return dm.div;

  // Round up
  return dm.div.negative !== 0 ? dm.div.isubn(1) : dm.div.iaddn(1);
};

BN.prototype.modn = function modn(num) {
  var p = (1 << 26) % num;

  var acc = 0;
  for (var i = this.length - 1; i >= 0; i--)
    acc = (p * acc + (this.words[i] | 0)) % num;

  return acc;
};

// In-place division by number
BN.prototype.idivn = function idivn(num) {
  var carry = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var w = (this.words[i] | 0) + carry * 0x4000000;
    this.words[i] = (w / num) | 0;
    carry = w % num;
  }

  return this.strip();
};

BN.prototype.divn = function divn(num) {
  return this.clone().idivn(num);
};

BN.prototype.isEven = function isEven() {
  return (this.words[0] & 1) === 0;
};

BN.prototype.isOdd = function isOdd() {
  return (this.words[0] & 1) === 1;
};

// And first word and num
BN.prototype.andln = function andln(num) {
  return this.words[0] & num;
};

BN.prototype.cmpn = function cmpn(num) {
  var negative = num < 0;
  if (negative)
    num = -num;

  if (this.negative !== 0 && !negative)
    return -1;
  else if (this.negative === 0 && negative)
    return 1;

  num &= 0x3ffffff;
  this.strip();

  var res;
  if (this.length > 1) {
    res = 1;
  } else {
    var w = this.words[0] | 0;
    res = w === num ? 0 : w < num ? -1 : 1;
  }
  if (this.negative !== 0)
    res = -res;
  return res;
};

// Compare two numbers and return:
// 1 - if `this` > `num`
// 0 - if `this` == `num`
// -1 - if `this` < `num`
BN.prototype.cmp = function cmp(num) {
  if (this.negative !== 0 && num.negative === 0)
    return -1;
  else if (this.negative === 0 && num.negative !== 0)
    return 1;

  var res = this.ucmp(num);
  if (this.negative !== 0)
    return -res;
  else
    return res;
};

// Unsigned comparison
BN.prototype.ucmp = function ucmp(num) {
  // At this point both numbers have the same sign
  if (this.length > num.length)
    return 1;
  else if (this.length < num.length)
    return -1;

  var res = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var a = this.words[i] | 0;
    var b = num.words[i] | 0;

    if (a === b)
      continue;
    if (a < b)
      res = -1;
    else if (a > b)
      res = 1;
    break;
  }
  return res;
};
})(undefined, __bn);

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return {_:0, a:0, b:undefined};
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return {_:0, a:1, b:val};
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
}

// TODO: inefficient compared to real fromInt?
__bn.Z = new __bn.BN(0);
__bn.ONE = new __bn.BN(1);
__bn.MOD32 = new __bn.BN(0x100000000); // 2^32
var I_fromNumber = function(x) {return new __bn.BN(x);}
var I_fromInt = I_fromNumber;
var I_fromBits = function(lo,hi) {
    var x = new __bn.BN(lo >>> 0);
    var y = new __bn.BN(hi >>> 0);
    y.ishln(32);
    x.iadd(y);
    return x;
}
var I_fromString = function(s) {return new __bn.BN(s);}
var I_toInt = function(x) {return I_toNumber(x.mod(__bn.MOD32));}
var I_toWord = function(x) {return I_toInt(x) >>> 0;};
// TODO: inefficient!
var I_toNumber = function(x) {return Number(x.toString());}
var I_equals = function(a,b) {return a.cmp(b) === 0;}
var I_compare = function(a,b) {return a.cmp(b);}
var I_compareInt = function(x,i) {return x.cmp(new __bn.BN(i));}
var I_negate = function(x) {return x.neg();}
var I_add = function(a,b) {return a.add(b);}
var I_sub = function(a,b) {return a.sub(b);}
var I_mul = function(a,b) {return a.mul(b);}
var I_mod = function(a,b) {return I_rem(I_add(b, I_rem(a, b)), b);}
var I_quotRem = function(a,b) {
    var qr = a.divmod(b);
    return {_:0, a:qr.div, b:qr.mod};
}
var I_div = function(a,b) {
    if((a.cmp(__bn.Z)>=0) != (a.cmp(__bn.Z)>=0)) {
        if(a.cmp(a.rem(b), __bn.Z) !== 0) {
            return a.div(b).sub(__bn.ONE);
        }
    }
    return a.div(b);
}
var I_divMod = function(a,b) {
    return {_:0, a:I_div(a,b), b:a.mod(b)};
}
var I_quot = function(a,b) {return a.div(b);}
var I_rem = function(a,b) {return a.mod(b);}
var I_and = function(a,b) {return a.and(b);}
var I_or = function(a,b) {return a.or(b);}
var I_xor = function(a,b) {return a.xor(b);}
var I_shiftLeft = function(a,b) {return a.shln(b);}
var I_shiftRight = function(a,b) {return a.shrn(b);}
var I_signum = function(x) {return x.cmp(new __bn.BN(0));}
var I_abs = function(x) {return x.abs();}
var I_decodeDouble = function(x) {
    var dec = decodeDouble(x);
    var mantissa = I_fromBits(dec.c, dec.b);
    if(dec.a < 0) {
        mantissa = I_negate(mantissa);
    }
    return {_:0, a:dec.d, b:mantissa};
}
var I_toString = function(x) {return x.toString();}
var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    if(x.isNegative()) {
        return I_negate(I_fromInt64(x.negate()));
    } else {
        return I_fromBits(x.low, x.high);
    }
}

function I_toInt64(x) {
    if(x.negative) {
        return I_toInt64(I_negate(x)).negate();
    } else {
        return new Long(I_toInt(x), I_toInt(I_shiftRight(x,32)));
    }
}

function I_fromWord64(x) {
    return I_fromBits(x.toInt(), x.shru(32).toInt());
}

function I_toWord64(x) {
    var w = I_toInt64(x);
    w.unsigned = true;
    return w;
}

/**
 * @license long.js (c) 2013 Daniel Wirtz <dcode@dcode.io>
 * Released under the Apache License, Version 2.0
 * see: https://github.com/dcodeIO/long.js for details
 */
function Long(low, high, unsigned) {
    this.low = low | 0;
    this.high = high | 0;
    this.unsigned = !!unsigned;
}

var INT_CACHE = {};
var UINT_CACHE = {};
function cacheable(x, u) {
    return u ? 0 <= (x >>>= 0) && x < 256 : -128 <= (x |= 0) && x < 128;
}

function __fromInt(value, unsigned) {
    var obj, cachedObj, cache;
    if (unsigned) {
        if (cache = cacheable(value >>>= 0, true)) {
            cachedObj = UINT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache)
            UINT_CACHE[value] = obj;
        return obj;
    } else {
        if (cache = cacheable(value |= 0, false)) {
            cachedObj = INT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, value < 0 ? -1 : 0, false);
        if (cache)
            INT_CACHE[value] = obj;
        return obj;
    }
}

function __fromNumber(value, unsigned) {
    if (isNaN(value) || !isFinite(value))
        return unsigned ? UZERO : ZERO;
    if (unsigned) {
        if (value < 0)
            return UZERO;
        if (value >= TWO_PWR_64_DBL)
            return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL)
            return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL)
            return MAX_VALUE;
    }
    if (value < 0)
        return __fromNumber(-value, unsigned).neg();
    return new Long((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
}
var pow_dbl = Math.pow;
var TWO_PWR_16_DBL = 1 << 16;
var TWO_PWR_24_DBL = 1 << 24;
var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
var TWO_PWR_24 = __fromInt(TWO_PWR_24_DBL);
var ZERO = __fromInt(0);
Long.ZERO = ZERO;
var UZERO = __fromInt(0, true);
Long.UZERO = UZERO;
var ONE = __fromInt(1);
Long.ONE = ONE;
var UONE = __fromInt(1, true);
Long.UONE = UONE;
var NEG_ONE = __fromInt(-1);
Long.NEG_ONE = NEG_ONE;
var MAX_VALUE = new Long(0xFFFFFFFF|0, 0x7FFFFFFF|0, false);
Long.MAX_VALUE = MAX_VALUE;
var MAX_UNSIGNED_VALUE = new Long(0xFFFFFFFF|0, 0xFFFFFFFF|0, true);
Long.MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
var MIN_VALUE = new Long(0, 0x80000000|0, false);
Long.MIN_VALUE = MIN_VALUE;
var __lp = Long.prototype;
__lp.toInt = function() {return this.unsigned ? this.low >>> 0 : this.low;};
__lp.toNumber = function() {
    if (this.unsigned)
        return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
    return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
};
__lp.isZero = function() {return this.high === 0 && this.low === 0;};
__lp.isNegative = function() {return !this.unsigned && this.high < 0;};
__lp.isOdd = function() {return (this.low & 1) === 1;};
__lp.eq = function(other) {
    if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
        return false;
    return this.high === other.high && this.low === other.low;
};
__lp.neq = function(other) {return !this.eq(other);};
__lp.lt = function(other) {return this.comp(other) < 0;};
__lp.lte = function(other) {return this.comp(other) <= 0;};
__lp.gt = function(other) {return this.comp(other) > 0;};
__lp.gte = function(other) {return this.comp(other) >= 0;};
__lp.compare = function(other) {
    if (this.eq(other))
        return 0;
    var thisNeg = this.isNegative(),
        otherNeg = other.isNegative();
    if (thisNeg && !otherNeg)
        return -1;
    if (!thisNeg && otherNeg)
        return 1;
    if (!this.unsigned)
        return this.sub(other).isNegative() ? -1 : 1;
    return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
};
__lp.comp = __lp.compare;
__lp.negate = function() {
    if (!this.unsigned && this.eq(MIN_VALUE))
        return MIN_VALUE;
    return this.not().add(ONE);
};
__lp.neg = __lp.negate;
__lp.add = function(addend) {
    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = addend.high >>> 16;
    var b32 = addend.high & 0xFFFF;
    var b16 = addend.low >>> 16;
    var b00 = addend.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 + b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 + b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 + b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 + b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.subtract = function(subtrahend) {return this.add(subtrahend.neg());};
__lp.sub = __lp.subtract;
__lp.multiply = function(multiplier) {
    if (this.isZero())
        return ZERO;
    if (multiplier.isZero())
        return ZERO;
    if (this.eq(MIN_VALUE))
        return multiplier.isOdd() ? MIN_VALUE : ZERO;
    if (multiplier.eq(MIN_VALUE))
        return this.isOdd() ? MIN_VALUE : ZERO;

    if (this.isNegative()) {
        if (multiplier.isNegative())
            return this.neg().mul(multiplier.neg());
        else
            return this.neg().mul(multiplier).neg();
    } else if (multiplier.isNegative())
        return this.mul(multiplier.neg()).neg();

    if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
        return __fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);

    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = multiplier.high >>> 16;
    var b32 = multiplier.high & 0xFFFF;
    var b16 = multiplier.low >>> 16;
    var b00 = multiplier.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 * b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 * b00;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c16 += a00 * b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 * b00;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a16 * b16;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a00 * b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.mul = __lp.multiply;
__lp.divide = function(divisor) {
    if (divisor.isZero())
        throw Error('division by zero');
    if (this.isZero())
        return this.unsigned ? UZERO : ZERO;
    var approx, rem, res;
    if (this.eq(MIN_VALUE)) {
        if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
            return MIN_VALUE;
        else if (divisor.eq(MIN_VALUE))
            return ONE;
        else {
            var halfThis = this.shr(1);
            approx = halfThis.div(divisor).shl(1);
            if (approx.eq(ZERO)) {
                return divisor.isNegative() ? ONE : NEG_ONE;
            } else {
                rem = this.sub(divisor.mul(approx));
                res = approx.add(rem.div(divisor));
                return res;
            }
        }
    } else if (divisor.eq(MIN_VALUE))
        return this.unsigned ? UZERO : ZERO;
    if (this.isNegative()) {
        if (divisor.isNegative())
            return this.neg().div(divisor.neg());
        return this.neg().div(divisor).neg();
    } else if (divisor.isNegative())
        return this.div(divisor.neg()).neg();

    res = ZERO;
    rem = this;
    while (rem.gte(divisor)) {
        approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
        var log2 = Math.ceil(Math.log(approx) / Math.LN2),
            delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48),
            approxRes = __fromNumber(approx),
            approxRem = approxRes.mul(divisor);
        while (approxRem.isNegative() || approxRem.gt(rem)) {
            approx -= delta;
            approxRes = __fromNumber(approx, this.unsigned);
            approxRem = approxRes.mul(divisor);
        }
        if (approxRes.isZero())
            approxRes = ONE;

        res = res.add(approxRes);
        rem = rem.sub(approxRem);
    }
    return res;
};
__lp.div = __lp.divide;
__lp.modulo = function(divisor) {return this.sub(this.div(divisor).mul(divisor));};
__lp.mod = __lp.modulo;
__lp.not = function not() {return new Long(~this.low, ~this.high, this.unsigned);};
__lp.and = function(other) {return new Long(this.low & other.low, this.high & other.high, this.unsigned);};
__lp.or = function(other) {return new Long(this.low | other.low, this.high | other.high, this.unsigned);};
__lp.xor = function(other) {return new Long(this.low ^ other.low, this.high ^ other.high, this.unsigned);};

__lp.shl = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
    else
        return new Long(0, this.low << (numBits - 32), this.unsigned);
};

__lp.shr = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
    else
        return new Long(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
};

__lp.shru = function(numBits) {
    numBits &= 63;
    if (numBits === 0)
        return this;
    else {
        var high = this.high;
        if (numBits < 32) {
            var low = this.low;
            return new Long((low >>> numBits) | (high << (32 - numBits)), high >>> numBits, this.unsigned);
        } else if (numBits === 32)
            return new Long(high, 0, this.unsigned);
        else
            return new Long(high >>> (numBits - 32), 0, this.unsigned);
    }
};

__lp.toSigned = function() {return this.unsigned ? new Long(this.low, this.high, false) : this;};
__lp.toUnsigned = function() {return this.unsigned ? this : new Long(this.low, this.high, true);};

// Int64
function hs_eqInt64(x, y) {return x.eq(y);}
function hs_neInt64(x, y) {return x.neq(y);}
function hs_ltInt64(x, y) {return x.lt(y);}
function hs_leInt64(x, y) {return x.lte(y);}
function hs_gtInt64(x, y) {return x.gt(y);}
function hs_geInt64(x, y) {return x.gte(y);}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shl(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shr(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shru(bits);}
function hs_int64ToInt(x) {return x.toInt();}
var hs_intToInt64 = __fromInt;

// Word64
function hs_wordToWord64(x) {return __fromInt(x, true);}
function hs_word64ToWord(x) {return x.toInt(x);}
function hs_mkWord64(low, high) {return new Long(low,high,true);}
function hs_and64(a,b) {return a.and(b);};
function hs_or64(a,b) {return a.or(b);};
function hs_xor64(a,b) {return a.xor(b);};
function hs_not64(x) {return x.not();}
var hs_eqWord64 = hs_eqInt64;
var hs_neWord64 = hs_neInt64;
var hs_ltWord64 = hs_ltInt64;
var hs_leWord64 = hs_leInt64;
var hs_gtWord64 = hs_gtInt64;
var hs_geWord64 = hs_geInt64;
var hs_quotWord64 = hs_quotInt64;
var hs_remWord64 = hs_remInt64;
var hs_uncheckedShiftL64 = hs_uncheckedIShiftL64;
var hs_uncheckedShiftRL64 = hs_uncheckedIShiftRL64;
function hs_int64ToWord64(x) {return x.toUnsigned();}
function hs_word64ToInt64(x) {return x.toSigned();}

// Joseph Myers' MD5 implementation, ported to work on typed arrays.
// Used under the BSD license.
function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s, n) {
    var a = s['v']['w8'];
    var orig_n = n,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=n; i+=64) {
        md5cycle(state, md5blk(a.subarray(i-64, i)));
    }
    a = a.subarray(i-64);
    n = n < (i-64) ? 0 : n-(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<n; i++)
        tail[i>>2] |= a[i] << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = orig_n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s[i]
            + (s[i+1] << 8)
            + (s[i+2] << 16)
            + (s[i+3] << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s, n) {
    return hex(md51(s, n));
}

window['md5'] = md5;

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

function __hsbase_MD5Init(ctx) {}
// Note that this is a one time "update", since that's all that's used by
// GHC.Fingerprint.
function __hsbase_MD5Update(ctx, s, n) {
    ctx.md5 = md51(s, n);
}
function __hsbase_MD5Final(out, ctx) {
    var a = out['v']['i32'];
    a[0] = ctx.md5[0];
    a[1] = ctx.md5[1];
    a[2] = ctx.md5[2];
    a[3] = ctx.md5[3];
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = new Array(n);
    for(var i = 0; i < n; ++i) {
        arr[i] = x;
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    return new ByteArray(new ArrayBuffer(n));
}

// Wrap a JS ArrayBuffer into a ByteArray. Truncates the array length to the
// closest multiple of 8 bytes.
function wrapByteArr(buffer) {
    var diff = buffer.byteLength % 8;
    if(diff != 0) {
        var buffer = buffer.slice(0, buffer.byteLength-diff);
    }
    return new ByteArray(buffer);
}

function ByteArray(buffer) {
    var views =
        { 'i8' : new Int8Array(buffer)
        , 'i16': new Int16Array(buffer)
        , 'i32': new Int32Array(buffer)
        , 'w8' : new Uint8Array(buffer)
        , 'w16': new Uint16Array(buffer)
        , 'w32': new Uint32Array(buffer)
        , 'f32': new Float32Array(buffer)
        , 'f64': new Float64Array(buffer)
        };
    this['b'] = buffer;
    this['v'] = views;
    this['off'] = 0;
}
window['newArr'] = newArr;
window['newByteArr'] = newByteArr;
window['wrapByteArr'] = wrapByteArr;
window['ByteArray'] = ByteArray;

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return {_:0, a:1, b:E(w).val};
}

function finalizeWeak(w) {
    return {_:0, a:B(A1(E(w).fin, __Z))};
}

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as._ === 1; as = as.b) {
        arr.push(as.a);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1,
            a:arr[elem],
            b:new T(function(){return __arr2lst(elem+1,arr);})};
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs._ === 1; xs = E(xs.b)) {
        arr.push(E(xs.a));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

var _0=function(_1,_2,_){var _3=B(A1(_1,_)),_4=B(A1(_2,_));return _3;},_5=function(_6,_7,_){var _8=B(A1(_6,_)),_9=B(A1(_7,_));return new T(function(){return B(A1(_8,_9));});},_a=function(_b,_c,_){var _d=B(A1(_c,_));return _b;},_e=function(_f,_g,_){var _h=B(A1(_g,_));return new T(function(){return B(A1(_f,_h));});},_i=new T2(0,_e,_a),_j=function(_k,_){return _k;},_l=function(_m,_n,_){var _o=B(A1(_m,_));return new F(function(){return A1(_n,_);});},_p=new T5(0,_i,_j,_5,_l,_0),_q=new T(function(){return B(unCStr("base"));}),_r=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_s=new T(function(){return B(unCStr("IOException"));}),_t=new T5(0,new Long(4053623282,1685460941,true),new Long(3693590983,2507416641,true),_q,_r,_s),_u=__Z,_v=new T5(0,new Long(4053623282,1685460941,true),new Long(3693590983,2507416641,true),_t,_u,_u),_w=function(_x){return E(_v);},_y=function(_z){return E(E(_z).a);},_A=function(_B,_C,_D){var _E=B(A1(_B,_)),_F=B(A1(_C,_)),_G=hs_eqWord64(_E.a,_F.a);if(!_G){return __Z;}else{var _H=hs_eqWord64(_E.b,_F.b);return (!_H)?__Z:new T1(1,_D);}},_I=function(_J){var _K=E(_J);return new F(function(){return _A(B(_y(_K.a)),_w,_K.b);});},_L=new T(function(){return B(unCStr(": "));}),_M=new T(function(){return B(unCStr(")"));}),_N=new T(function(){return B(unCStr(" ("));}),_O=function(_P,_Q){var _R=E(_P);return (_R._==0)?E(_Q):new T2(1,_R.a,new T(function(){return B(_O(_R.b,_Q));}));},_S=new T(function(){return B(unCStr("interrupted"));}),_T=new T(function(){return B(unCStr("system error"));}),_U=new T(function(){return B(unCStr("unsatisified constraints"));}),_V=new T(function(){return B(unCStr("user error"));}),_W=new T(function(){return B(unCStr("permission denied"));}),_X=new T(function(){return B(unCStr("illegal operation"));}),_Y=new T(function(){return B(unCStr("end of file"));}),_Z=new T(function(){return B(unCStr("resource exhausted"));}),_10=new T(function(){return B(unCStr("resource busy"));}),_11=new T(function(){return B(unCStr("does not exist"));}),_12=new T(function(){return B(unCStr("already exists"));}),_13=new T(function(){return B(unCStr("resource vanished"));}),_14=new T(function(){return B(unCStr("timeout"));}),_15=new T(function(){return B(unCStr("unsupported operation"));}),_16=new T(function(){return B(unCStr("hardware fault"));}),_17=new T(function(){return B(unCStr("inappropriate type"));}),_18=new T(function(){return B(unCStr("invalid argument"));}),_19=new T(function(){return B(unCStr("failed"));}),_1a=new T(function(){return B(unCStr("protocol error"));}),_1b=function(_1c,_1d){switch(E(_1c)){case 0:return new F(function(){return _O(_12,_1d);});break;case 1:return new F(function(){return _O(_11,_1d);});break;case 2:return new F(function(){return _O(_10,_1d);});break;case 3:return new F(function(){return _O(_Z,_1d);});break;case 4:return new F(function(){return _O(_Y,_1d);});break;case 5:return new F(function(){return _O(_X,_1d);});break;case 6:return new F(function(){return _O(_W,_1d);});break;case 7:return new F(function(){return _O(_V,_1d);});break;case 8:return new F(function(){return _O(_U,_1d);});break;case 9:return new F(function(){return _O(_T,_1d);});break;case 10:return new F(function(){return _O(_1a,_1d);});break;case 11:return new F(function(){return _O(_19,_1d);});break;case 12:return new F(function(){return _O(_18,_1d);});break;case 13:return new F(function(){return _O(_17,_1d);});break;case 14:return new F(function(){return _O(_16,_1d);});break;case 15:return new F(function(){return _O(_15,_1d);});break;case 16:return new F(function(){return _O(_14,_1d);});break;case 17:return new F(function(){return _O(_13,_1d);});break;default:return new F(function(){return _O(_S,_1d);});}},_1e=new T(function(){return B(unCStr("}"));}),_1f=new T(function(){return B(unCStr("{handle: "));}),_1g=function(_1h,_1i,_1j,_1k,_1l,_1m){var _1n=new T(function(){var _1o=new T(function(){var _1p=new T(function(){var _1q=E(_1k);if(!_1q._){return E(_1m);}else{var _1r=new T(function(){return B(_O(_1q,new T(function(){return B(_O(_M,_1m));},1)));},1);return B(_O(_N,_1r));}},1);return B(_1b(_1i,_1p));}),_1s=E(_1j);if(!_1s._){return E(_1o);}else{return B(_O(_1s,new T(function(){return B(_O(_L,_1o));},1)));}}),_1t=E(_1l);if(!_1t._){var _1u=E(_1h);if(!_1u._){return E(_1n);}else{var _1v=E(_1u.a);if(!_1v._){var _1w=new T(function(){var _1x=new T(function(){return B(_O(_1e,new T(function(){return B(_O(_L,_1n));},1)));},1);return B(_O(_1v.a,_1x));},1);return new F(function(){return _O(_1f,_1w);});}else{var _1y=new T(function(){var _1z=new T(function(){return B(_O(_1e,new T(function(){return B(_O(_L,_1n));},1)));},1);return B(_O(_1v.a,_1z));},1);return new F(function(){return _O(_1f,_1y);});}}}else{return new F(function(){return _O(_1t.a,new T(function(){return B(_O(_L,_1n));},1));});}},_1A=function(_1B){var _1C=E(_1B);return new F(function(){return _1g(_1C.a,_1C.b,_1C.c,_1C.d,_1C.f,_u);});},_1D=function(_1E){return new T2(0,_1F,_1E);},_1G=function(_1H,_1I,_1J){var _1K=E(_1I);return new F(function(){return _1g(_1K.a,_1K.b,_1K.c,_1K.d,_1K.f,_1J);});},_1L=function(_1M,_1N){var _1O=E(_1M);return new F(function(){return _1g(_1O.a,_1O.b,_1O.c,_1O.d,_1O.f,_1N);});},_1P=44,_1Q=93,_1R=91,_1S=function(_1T,_1U,_1V){var _1W=E(_1U);if(!_1W._){return new F(function(){return unAppCStr("[]",_1V);});}else{var _1X=new T(function(){var _1Y=new T(function(){var _1Z=function(_20){var _21=E(_20);if(!_21._){return E(new T2(1,_1Q,_1V));}else{var _22=new T(function(){return B(A2(_1T,_21.a,new T(function(){return B(_1Z(_21.b));})));});return new T2(1,_1P,_22);}};return B(_1Z(_1W.b));});return B(A2(_1T,_1W.a,_1Y));});return new T2(1,_1R,_1X);}},_23=function(_24,_25){return new F(function(){return _1S(_1L,_24,_25);});},_26=new T3(0,_1G,_1A,_23),_1F=new T(function(){return new T5(0,_w,_26,_1D,_I,_1A);}),_27=new T(function(){return E(_1F);}),_28=function(_29){return E(E(_29).c);},_2a=__Z,_2b=7,_2c=function(_2d){return new T6(0,_2a,_2b,_u,_2d,_2a,_2a);},_2e=function(_2f,_){var _2g=new T(function(){return B(A2(_28,_27,new T(function(){return B(A1(_2c,_2f));})));});return new F(function(){return die(_2g);});},_2h=function(_2i,_){return new F(function(){return _2e(_2i,_);});},_2j=function(_2k){return new F(function(){return A1(_2h,_2k);});},_2l=function(_2m,_2n,_){var _2o=B(A1(_2m,_));return new F(function(){return A2(_2n,_2o,_);});},_2p=new T5(0,_p,_2l,_l,_j,_2j),_2q=function(_2r){return E(_2r);},_2s=new T2(0,_2p,_2q),_2t=0,_2u=function(_){return _2t;},_2v=function(_2w,_2x,_){return new F(function(){return _2u(_);});},_2y="scroll",_2z="submit",_2A="blur",_2B="focus",_2C="change",_2D="unload",_2E="load",_2F=function(_2G){switch(E(_2G)){case 0:return E(_2E);case 1:return E(_2D);case 2:return E(_2C);case 3:return E(_2B);case 4:return E(_2A);case 5:return E(_2z);default:return E(_2y);}},_2H=new T2(0,_2F,_2v),_2I="metaKey",_2J="shiftKey",_2K="altKey",_2L="ctrlKey",_2M="keyCode",_2N=function(_2O,_){var _2P=__get(_2O,E(_2M)),_2Q=__get(_2O,E(_2L)),_2R=__get(_2O,E(_2K)),_2S=__get(_2O,E(_2J)),_2T=__get(_2O,E(_2I));return new T(function(){var _2U=Number(_2P),_2V=jsTrunc(_2U);return new T5(0,_2V,E(_2Q),E(_2R),E(_2S),E(_2T));});},_2W=function(_2X,_2Y,_){return new F(function(){return _2N(E(_2Y),_);});},_2Z="keydown",_30="keyup",_31="keypress",_32=function(_33){switch(E(_33)){case 0:return E(_31);case 1:return E(_30);default:return E(_2Z);}},_34=new T2(0,_32,_2W),_35="deltaZ",_36="deltaY",_37="deltaX",_38=function(_39,_3a){var _3b=jsShowI(_39);return new F(function(){return _O(fromJSStr(_3b),_3a);});},_3c=41,_3d=40,_3e=function(_3f,_3g,_3h){if(_3g>=0){return new F(function(){return _38(_3g,_3h);});}else{if(_3f<=6){return new F(function(){return _38(_3g,_3h);});}else{return new T2(1,_3d,new T(function(){var _3i=jsShowI(_3g);return B(_O(fromJSStr(_3i),new T2(1,_3c,_3h)));}));}}},_3j=new T(function(){return B(unCStr(")"));}),_3k=new T(function(){return B(_3e(0,2,_3j));}),_3l=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_3k));}),_3m=function(_3n){return new F(function(){return err(B(unAppCStr("toEnum{MouseButton}: tag (",new T(function(){return B(_3e(0,_3n,_3l));}))));});},_3o=function(_3p,_){return new T(function(){var _3q=Number(E(_3p)),_3r=jsTrunc(_3q);if(_3r<0){return B(_3m(_3r));}else{if(_3r>2){return B(_3m(_3r));}else{return _3r;}}});},_3s=0,_3t=new T3(0,_3s,_3s,_3s),_3u="button",_3v=new T(function(){return eval("jsGetMouseCoords");}),_3w=function(_3x,_){var _3y=E(_3x);if(!_3y._){return _u;}else{var _3z=B(_3w(_3y.b,_));return new T2(1,new T(function(){var _3A=Number(E(_3y.a));return jsTrunc(_3A);}),_3z);}},_3B=function(_3C,_){var _3D=__arr2lst(0,_3C);return new F(function(){return _3w(_3D,_);});},_3E=function(_3F,_){return new F(function(){return _3B(E(_3F),_);});},_3G=function(_3H,_){return new T(function(){var _3I=Number(E(_3H));return jsTrunc(_3I);});},_3J=new T2(0,_3G,_3E),_3K=function(_3L,_){var _3M=E(_3L);if(!_3M._){return _u;}else{var _3N=B(_3K(_3M.b,_));return new T2(1,_3M.a,_3N);}},_3O=new T(function(){return B(unCStr("Pattern match failure in do expression at src/Haste/Prim/Any.hs:272:5-9"));}),_3P=new T6(0,_2a,_2b,_u,_3O,_2a,_2a),_3Q=new T(function(){return B(_1D(_3P));}),_3R=function(_){return new F(function(){return die(_3Q);});},_3S=function(_3T){return E(E(_3T).a);},_3U=function(_3V,_3W,_3X,_){var _3Y=__arr2lst(0,_3X),_3Z=B(_3K(_3Y,_)),_40=E(_3Z);if(!_40._){return new F(function(){return _3R(_);});}else{var _41=E(_40.b);if(!_41._){return new F(function(){return _3R(_);});}else{if(!E(_41.b)._){var _42=B(A3(_3S,_3V,_40.a,_)),_43=B(A3(_3S,_3W,_41.a,_));return new T2(0,_42,_43);}else{return new F(function(){return _3R(_);});}}}},_44=function(_){return new F(function(){return __jsNull();});},_45=function(_46){var _47=B(A1(_46,_));return E(_47);},_48=new T(function(){return B(_45(_44));}),_49=new T(function(){return E(_48);}),_4a=function(_4b,_4c,_){if(E(_4b)==7){var _4d=__app1(E(_3v),_4c),_4e=B(_3U(_3J,_3J,_4d,_)),_4f=__get(_4c,E(_37)),_4g=__get(_4c,E(_36)),_4h=__get(_4c,E(_35));return new T(function(){return new T3(0,E(_4e),E(_2a),E(new T3(0,_4f,_4g,_4h)));});}else{var _4i=__app1(E(_3v),_4c),_4j=B(_3U(_3J,_3J,_4i,_)),_4k=__get(_4c,E(_3u)),_4l=__eq(_4k,E(_49));if(!E(_4l)){var _4m=B(_3o(_4k,_));return new T(function(){return new T3(0,E(_4j),E(new T1(1,_4m)),E(_3t));});}else{return new T(function(){return new T3(0,E(_4j),E(_2a),E(_3t));});}}},_4n=function(_4o,_4p,_){return new F(function(){return _4a(_4o,E(_4p),_);});},_4q="mouseout",_4r="mouseover",_4s="mousemove",_4t="mouseup",_4u="mousedown",_4v="dblclick",_4w="click",_4x="wheel",_4y=function(_4z){switch(E(_4z)){case 0:return E(_4w);case 1:return E(_4v);case 2:return E(_4u);case 3:return E(_4t);case 4:return E(_4s);case 5:return E(_4r);case 6:return E(_4q);default:return E(_4x);}},_4A=new T2(0,_4y,_4n),_4B=function(_){return _2t;},_4C=function(_4D,_){return new T1(1,_4D);},_4E=new T2(0,_2q,_4C),_4F=new T2(0,_2s,_j),_4G=new T(function(){return B(unCStr("base"));}),_4H=new T(function(){return B(unCStr("Control.Exception.Base"));}),_4I=new T(function(){return B(unCStr("PatternMatchFail"));}),_4J=new T5(0,new Long(18445595,3739165398,true),new Long(52003073,3246954884,true),_4G,_4H,_4I),_4K=new T5(0,new Long(18445595,3739165398,true),new Long(52003073,3246954884,true),_4J,_u,_u),_4L=function(_4M){return E(_4K);},_4N=function(_4O){var _4P=E(_4O);return new F(function(){return _A(B(_y(_4P.a)),_4L,_4P.b);});},_4Q=function(_4R){return E(E(_4R).a);},_4S=function(_4T){return new T2(0,_4U,_4T);},_4V=function(_4W,_4X){return new F(function(){return _O(E(_4W).a,_4X);});},_4Y=function(_4Z,_50){return new F(function(){return _1S(_4V,_4Z,_50);});},_51=function(_52,_53,_54){return new F(function(){return _O(E(_53).a,_54);});},_55=new T3(0,_51,_4Q,_4Y),_4U=new T(function(){return new T5(0,_4L,_55,_4S,_4N,_4Q);}),_56=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_57=function(_58,_59){return new F(function(){return die(new T(function(){return B(A2(_28,_59,_58));}));});},_5a=function(_5b,_5c){return new F(function(){return _57(_5b,_5c);});},_5d=function(_5e,_5f){var _5g=E(_5f);if(!_5g._){return new T2(0,_u,_u);}else{var _5h=_5g.a;if(!B(A1(_5e,_5h))){return new T2(0,_u,_5g);}else{var _5i=new T(function(){var _5j=B(_5d(_5e,_5g.b));return new T2(0,_5j.a,_5j.b);});return new T2(0,new T2(1,_5h,new T(function(){return E(E(_5i).a);})),new T(function(){return E(E(_5i).b);}));}}},_5k=32,_5l=new T(function(){return B(unCStr("\n"));}),_5m=function(_5n){return (E(_5n)==124)?false:true;},_5o=function(_5p,_5q){var _5r=B(_5d(_5m,B(unCStr(_5p)))),_5s=_5r.a,_5t=function(_5u,_5v){var _5w=new T(function(){var _5x=new T(function(){return B(_O(_5q,new T(function(){return B(_O(_5v,_5l));},1)));});return B(unAppCStr(": ",_5x));},1);return new F(function(){return _O(_5u,_5w);});},_5y=E(_5r.b);if(!_5y._){return new F(function(){return _5t(_5s,_u);});}else{if(E(_5y.a)==124){return new F(function(){return _5t(_5s,new T2(1,_5k,_5y.b));});}else{return new F(function(){return _5t(_5s,_u);});}}},_5z=function(_5A){return new F(function(){return _5a(new T1(0,new T(function(){return B(_5o(_5A,_56));})),_4U);});},_5B=function(_){return new F(function(){return _5z("Main.hs:(22,1)-(49,30)|function bcodexMain");});},_5C=2,_5D=0,_5E=1,_5F=new T(function(){return B(unCStr("\'>"));}),_5G=new T(function(){return B(unCStr("<span class=\'"));}),_5H=new T(function(){return B(unCStr("&gt;"));}),_5I=new T(function(){return B(unCStr("&lt;"));}),_5J=new T(function(){return B(unCStr("&amp;"));}),_5K=new T(function(){return B(unCStr("</span>"));}),_5L=function(_5M){var _5N=E(_5M);if(!_5N._){return E(_5K);}else{var _5O=_5N.b,_5P=E(_5N.a);switch(E(_5P)){case 38:return new F(function(){return _O(_5J,new T(function(){return B(_5L(_5O));},1));});break;case 60:return new F(function(){return _O(_5I,new T(function(){return B(_5L(_5O));},1));});break;case 62:return new F(function(){return _O(_5H,new T(function(){return B(_5L(_5O));},1));});break;default:return new T2(1,_5P,new T(function(){return B(_5L(_5O));}));}}},_5Q=function(_5R,_5S){var _5T=new T(function(){var _5U=new T(function(){return B(_O(_5F,new T(function(){return B(_5L(_5S));},1)));},1);return B(_O(_5R,_5U));},1);return new F(function(){return _O(_5G,_5T);});},_5V=new T(function(){return B(unCStr("rs"));}),_5W=function(_5X){return new F(function(){return _5Q(_5V,_5X);});},_5Y=function(_5Z){return new F(function(){return _3e(0,E(_5Z),_u);});},_60=new T(function(){return B(unCStr("ri"));}),_61=function(_62){return new F(function(){return _5Q(_60,new T(function(){return B(_5Y(_62));},1));});},_63=new T(function(){return B(unCStr("]"));}),_64=new T(function(){return B(unCStr("bi"));}),_65=new T(function(){return B(unCStr("de"));}),_66=new T(function(){return B(unCStr("ex"));}),_67=new T(function(){return B(unCStr("}"));}),_68=new T(function(){return B(unCStr("bs"));}),_69=function(_6a){var _6b=E(_6a);switch(_6b._){case 0:var _6c=new T(function(){return B(unAppCStr("{",new T(function(){return B(_O(_6b.a,_67));})));},1);return new F(function(){return _5Q(_68,_6c);});break;case 1:return new F(function(){return _5Q(_66,_6b.a);});break;case 2:return new F(function(){return _5Q(_65,_6b.a);});break;default:var _6d=new T(function(){return B(unAppCStr("[",new T(function(){return B(_O(B(_3e(0,E(_6b.a),_u)),_63));})));},1);return new F(function(){return _5Q(_64,_6d);});}},_6e=function(_6f,_6g,_6h){var _6i=function(_6j){var _6k=E(_6j);if(!_6k._){return __Z;}else{var _6l=_6k.b,_6m=E(_6k.a);if(!_6m._){return new F(function(){return _O(B(_69(_6m.a)),new T(function(){return B(_6i(_6l));},1));});}else{return new F(function(){return _O(B(A1(_6f,_6m.a)),new T(function(){return B(_6i(_6l));},1));});}}};return new F(function(){return _6i(B(A1(_6g,new T2(1,new T1(1,_6h),_u))));});},_6n=function(_6o,_6p){return imul(E(_6o),E(_6p))|0;},_6q=function(_6r,_6s){return E(_6r)+E(_6s)|0;},_6t=new T0(1),_6u=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_6v=function(_6w){return new F(function(){return err(_6u);});},_6x=new T(function(){return B(_6v(_));}),_6y=function(_6z,_6A,_6B,_6C){var _6D=E(_6B);if(!_6D._){var _6E=_6D.a,_6F=E(_6C);if(!_6F._){var _6G=_6F.a,_6H=_6F.b,_6I=_6F.c;if(_6G<=(imul(3,_6E)|0)){return new T5(0,(1+_6E|0)+_6G|0,E(_6z),_6A,E(_6D),E(_6F));}else{var _6J=E(_6F.d);if(!_6J._){var _6K=_6J.a,_6L=_6J.b,_6M=_6J.c,_6N=_6J.d,_6O=E(_6F.e);if(!_6O._){var _6P=_6O.a;if(_6K>=(imul(2,_6P)|0)){var _6Q=function(_6R){var _6S=E(_6z),_6T=E(_6J.e);return (_6T._==0)?new T5(0,(1+_6E|0)+_6G|0,E(_6L),_6M,E(new T5(0,(1+_6E|0)+_6R|0,E(_6S),_6A,E(_6D),E(_6N))),E(new T5(0,(1+_6P|0)+_6T.a|0,E(_6H),_6I,E(_6T),E(_6O)))):new T5(0,(1+_6E|0)+_6G|0,E(_6L),_6M,E(new T5(0,(1+_6E|0)+_6R|0,E(_6S),_6A,E(_6D),E(_6N))),E(new T5(0,1+_6P|0,E(_6H),_6I,E(_6t),E(_6O))));},_6U=E(_6N);if(!_6U._){return new F(function(){return _6Q(_6U.a);});}else{return new F(function(){return _6Q(0);});}}else{return new T5(0,(1+_6E|0)+_6G|0,E(_6H),_6I,E(new T5(0,(1+_6E|0)+_6K|0,E(_6z),_6A,E(_6D),E(_6J))),E(_6O));}}else{return E(_6x);}}else{return E(_6x);}}}else{return new T5(0,1+_6E|0,E(_6z),_6A,E(_6D),E(_6t));}}else{var _6V=E(_6C);if(!_6V._){var _6W=_6V.a,_6X=_6V.b,_6Y=_6V.c,_6Z=_6V.e,_70=E(_6V.d);if(!_70._){var _71=_70.a,_72=_70.b,_73=_70.c,_74=_70.d,_75=E(_6Z);if(!_75._){var _76=_75.a;if(_71>=(imul(2,_76)|0)){var _77=function(_78){var _79=E(_6z),_7a=E(_70.e);return (_7a._==0)?new T5(0,1+_6W|0,E(_72),_73,E(new T5(0,1+_78|0,E(_79),_6A,E(_6t),E(_74))),E(new T5(0,(1+_76|0)+_7a.a|0,E(_6X),_6Y,E(_7a),E(_75)))):new T5(0,1+_6W|0,E(_72),_73,E(new T5(0,1+_78|0,E(_79),_6A,E(_6t),E(_74))),E(new T5(0,1+_76|0,E(_6X),_6Y,E(_6t),E(_75))));},_7b=E(_74);if(!_7b._){return new F(function(){return _77(_7b.a);});}else{return new F(function(){return _77(0);});}}else{return new T5(0,1+_6W|0,E(_6X),_6Y,E(new T5(0,1+_71|0,E(_6z),_6A,E(_6t),E(_70))),E(_75));}}else{return new T5(0,3,E(_72),_73,E(new T5(0,1,E(_6z),_6A,E(_6t),E(_6t))),E(new T5(0,1,E(_6X),_6Y,E(_6t),E(_6t))));}}else{var _7c=E(_6Z);return (_7c._==0)?new T5(0,3,E(_6X),_6Y,E(new T5(0,1,E(_6z),_6A,E(_6t),E(_6t))),E(_7c)):new T5(0,2,E(_6z),_6A,E(_6t),E(_6V));}}else{return new T5(0,1,E(_6z),_6A,E(_6t),E(_6t));}}},_7d=function(_7e,_7f){return new T5(0,1,E(_7e),_7f,E(_6t),E(_6t));},_7g=function(_7h,_7i,_7j){var _7k=E(_7j);if(!_7k._){return new F(function(){return _6y(_7k.b,_7k.c,_7k.d,B(_7g(_7h,_7i,_7k.e)));});}else{return new F(function(){return _7d(_7h,_7i);});}},_7l=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_7m=function(_7n){return new F(function(){return err(_7l);});},_7o=new T(function(){return B(_7m(_));}),_7p=function(_7q,_7r,_7s,_7t){var _7u=E(_7t);if(!_7u._){var _7v=_7u.a,_7w=E(_7s);if(!_7w._){var _7x=_7w.a,_7y=_7w.b,_7z=_7w.c;if(_7x<=(imul(3,_7v)|0)){return new T5(0,(1+_7x|0)+_7v|0,E(_7q),_7r,E(_7w),E(_7u));}else{var _7A=E(_7w.d);if(!_7A._){var _7B=_7A.a,_7C=E(_7w.e);if(!_7C._){var _7D=_7C.a,_7E=_7C.b,_7F=_7C.c,_7G=_7C.d;if(_7D>=(imul(2,_7B)|0)){var _7H=function(_7I){var _7J=E(_7C.e);return (_7J._==0)?new T5(0,(1+_7x|0)+_7v|0,E(_7E),_7F,E(new T5(0,(1+_7B|0)+_7I|0,E(_7y),_7z,E(_7A),E(_7G))),E(new T5(0,(1+_7v|0)+_7J.a|0,E(_7q),_7r,E(_7J),E(_7u)))):new T5(0,(1+_7x|0)+_7v|0,E(_7E),_7F,E(new T5(0,(1+_7B|0)+_7I|0,E(_7y),_7z,E(_7A),E(_7G))),E(new T5(0,1+_7v|0,E(_7q),_7r,E(_6t),E(_7u))));},_7K=E(_7G);if(!_7K._){return new F(function(){return _7H(_7K.a);});}else{return new F(function(){return _7H(0);});}}else{return new T5(0,(1+_7x|0)+_7v|0,E(_7y),_7z,E(_7A),E(new T5(0,(1+_7v|0)+_7D|0,E(_7q),_7r,E(_7C),E(_7u))));}}else{return E(_7o);}}else{return E(_7o);}}}else{return new T5(0,1+_7v|0,E(_7q),_7r,E(_6t),E(_7u));}}else{var _7L=E(_7s);if(!_7L._){var _7M=_7L.a,_7N=_7L.b,_7O=_7L.c,_7P=_7L.e,_7Q=E(_7L.d);if(!_7Q._){var _7R=_7Q.a,_7S=E(_7P);if(!_7S._){var _7T=_7S.a,_7U=_7S.b,_7V=_7S.c,_7W=_7S.d;if(_7T>=(imul(2,_7R)|0)){var _7X=function(_7Y){var _7Z=E(_7S.e);return (_7Z._==0)?new T5(0,1+_7M|0,E(_7U),_7V,E(new T5(0,(1+_7R|0)+_7Y|0,E(_7N),_7O,E(_7Q),E(_7W))),E(new T5(0,1+_7Z.a|0,E(_7q),_7r,E(_7Z),E(_6t)))):new T5(0,1+_7M|0,E(_7U),_7V,E(new T5(0,(1+_7R|0)+_7Y|0,E(_7N),_7O,E(_7Q),E(_7W))),E(new T5(0,1,E(_7q),_7r,E(_6t),E(_6t))));},_80=E(_7W);if(!_80._){return new F(function(){return _7X(_80.a);});}else{return new F(function(){return _7X(0);});}}else{return new T5(0,1+_7M|0,E(_7N),_7O,E(_7Q),E(new T5(0,1+_7T|0,E(_7q),_7r,E(_7S),E(_6t))));}}else{return new T5(0,3,E(_7N),_7O,E(_7Q),E(new T5(0,1,E(_7q),_7r,E(_6t),E(_6t))));}}else{var _81=E(_7P);return (_81._==0)?new T5(0,3,E(_81.b),_81.c,E(new T5(0,1,E(_7N),_7O,E(_6t),E(_6t))),E(new T5(0,1,E(_7q),_7r,E(_6t),E(_6t)))):new T5(0,2,E(_7q),_7r,E(_7L),E(_6t));}}else{return new T5(0,1,E(_7q),_7r,E(_6t),E(_6t));}}},_82=function(_83,_84,_85){var _86=E(_85);if(!_86._){return new F(function(){return _7p(_86.b,_86.c,B(_82(_83,_84,_86.d)),_86.e);});}else{return new F(function(){return _7d(_83,_84);});}},_87=function(_88,_89,_8a,_8b,_8c,_8d,_8e){return new F(function(){return _7p(_8b,_8c,B(_82(_88,_89,_8d)),_8e);});},_8f=function(_8g,_8h,_8i,_8j,_8k,_8l,_8m,_8n){var _8o=E(_8i);if(!_8o._){var _8p=_8o.a,_8q=_8o.b,_8r=_8o.c,_8s=_8o.d,_8t=_8o.e;if((imul(3,_8p)|0)>=_8j){if((imul(3,_8j)|0)>=_8p){return new T5(0,(_8p+_8j|0)+1|0,E(_8g),_8h,E(_8o),E(new T5(0,_8j,E(_8k),_8l,E(_8m),E(_8n))));}else{return new F(function(){return _6y(_8q,_8r,_8s,B(_8f(_8g,_8h,_8t,_8j,_8k,_8l,_8m,_8n)));});}}else{return new F(function(){return _7p(_8k,_8l,B(_8u(_8g,_8h,_8p,_8q,_8r,_8s,_8t,_8m)),_8n);});}}else{return new F(function(){return _87(_8g,_8h,_8j,_8k,_8l,_8m,_8n);});}},_8u=function(_8v,_8w,_8x,_8y,_8z,_8A,_8B,_8C){var _8D=E(_8C);if(!_8D._){var _8E=_8D.a,_8F=_8D.b,_8G=_8D.c,_8H=_8D.d,_8I=_8D.e;if((imul(3,_8x)|0)>=_8E){if((imul(3,_8E)|0)>=_8x){return new T5(0,(_8x+_8E|0)+1|0,E(_8v),_8w,E(new T5(0,_8x,E(_8y),_8z,E(_8A),E(_8B))),E(_8D));}else{return new F(function(){return _6y(_8y,_8z,_8A,B(_8f(_8v,_8w,_8B,_8E,_8F,_8G,_8H,_8I)));});}}else{return new F(function(){return _7p(_8F,_8G,B(_8u(_8v,_8w,_8x,_8y,_8z,_8A,_8B,_8H)),_8I);});}}else{return new F(function(){return _7g(_8v,_8w,new T5(0,_8x,E(_8y),_8z,E(_8A),E(_8B)));});}},_8J=function(_8K,_8L,_8M,_8N){var _8O=E(_8M);if(!_8O._){var _8P=_8O.a,_8Q=_8O.b,_8R=_8O.c,_8S=_8O.d,_8T=_8O.e,_8U=E(_8N);if(!_8U._){var _8V=_8U.a,_8W=_8U.b,_8X=_8U.c,_8Y=_8U.d,_8Z=_8U.e;if((imul(3,_8P)|0)>=_8V){if((imul(3,_8V)|0)>=_8P){return new T5(0,(_8P+_8V|0)+1|0,E(_8K),_8L,E(_8O),E(_8U));}else{return new F(function(){return _6y(_8Q,_8R,_8S,B(_8f(_8K,_8L,_8T,_8V,_8W,_8X,_8Y,_8Z)));});}}else{return new F(function(){return _7p(_8W,_8X,B(_8u(_8K,_8L,_8P,_8Q,_8R,_8S,_8T,_8Y)),_8Z);});}}else{return new F(function(){return _7g(_8K,_8L,_8O);});}}else{return new F(function(){return _82(_8K,_8L,_8N);});}},_90=function(_91,_92,_93,_94){var _95=E(_91);if(_95==1){var _96=E(_94);return (_96._==0)?new T3(0,new T5(0,1,E(_92),_93,E(_6t),E(_6t)),_u,_u):(_92<E(E(_96.a).a))?new T3(0,new T5(0,1,E(_92),_93,E(_6t),E(_6t)),_96,_u):new T3(0,new T5(0,1,E(_92),_93,E(_6t),E(_6t)),_u,_96);}else{var _97=B(_90(_95>>1,_92,_93,_94)),_98=_97.a,_99=_97.c,_9a=E(_97.b);if(!_9a._){return new T3(0,_98,_u,_99);}else{var _9b=E(_9a.a),_9c=_9b.a,_9d=_9b.b,_9e=E(_9a.b);if(!_9e._){return new T3(0,new T(function(){return B(_7g(_9c,_9d,_98));}),_u,_99);}else{var _9f=E(_9e.a),_9g=E(_9c),_9h=E(_9f.a);if(_9g<_9h){var _9i=B(_90(_95>>1,_9h,_9f.b,_9e.b));return new T3(0,new T(function(){return B(_8J(_9g,_9d,_98,_9i.a));}),_9i.b,_9i.c);}else{return new T3(0,_98,_u,_9a);}}}}},_9j=function(_9k,_9l,_9m){var _9n=E(_9m);if(!_9n._){var _9o=_9n.c,_9p=_9n.d,_9q=_9n.e,_9r=E(_9n.b);if(_9k!=_9r){if(_9k>_9r){return new F(function(){return _6y(_9r,_9o,_9p,B(_9j(_9k,_9l,_9q)));});}else{return new F(function(){return _7p(_9r,_9o,B(_9j(_9k,_9l,_9p)),_9q);});}}else{return new T5(0,_9n.a,E(_9k),_9l,E(_9p),E(_9q));}}else{return new T5(0,1,E(_9k),_9l,E(_6t),E(_6t));}},_9s=function(_9t,_9u){while(1){var _9v=E(_9u);if(!_9v._){return E(_9t);}else{var _9w=E(_9v.a),_9x=B(_9j(E(_9w.a),_9w.b,_9t));_9t=_9x;_9u=_9v.b;continue;}}},_9y=function(_9z,_9A,_9B,_9C){return new F(function(){return _9s(B(_9j(_9A,_9B,_9z)),_9C);});},_9D=function(_9E,_9F,_9G){var _9H=E(_9F);return new F(function(){return _9s(B(_9j(E(_9H.a),_9H.b,_9E)),_9G);});},_9I=function(_9J,_9K,_9L){while(1){var _9M=E(_9L);if(!_9M._){return E(_9K);}else{var _9N=E(_9M.a),_9O=_9N.a,_9P=_9N.b,_9Q=E(_9M.b);if(!_9Q._){return new F(function(){return _7g(_9O,_9P,_9K);});}else{var _9R=E(_9Q.a),_9S=E(_9O),_9T=E(_9R.a);if(_9S<_9T){var _9U=B(_90(_9J,_9T,_9R.b,_9Q.b)),_9V=_9U.a,_9W=E(_9U.c);if(!_9W._){var _9X=_9J<<1,_9Y=B(_8J(_9S,_9P,_9K,_9V));_9J=_9X;_9K=_9Y;_9L=_9U.b;continue;}else{return new F(function(){return _9D(B(_8J(_9S,_9P,_9K,_9V)),_9W.a,_9W.b);});}}else{return new F(function(){return _9y(_9K,_9S,_9P,_9Q);});}}}}},_9Z=function(_a0,_a1,_a2,_a3,_a4){var _a5=E(_a4);if(!_a5._){return new F(function(){return _7g(_a2,_a3,_a1);});}else{var _a6=E(_a5.a),_a7=E(_a6.a);if(_a2<_a7){var _a8=B(_90(_a0,_a7,_a6.b,_a5.b)),_a9=_a8.a,_aa=E(_a8.c);if(!_aa._){return new F(function(){return _9I(_a0<<1,B(_8J(_a2,_a3,_a1,_a9)),_a8.b);});}else{return new F(function(){return _9D(B(_8J(_a2,_a3,_a1,_a9)),_aa.a,_aa.b);});}}else{return new F(function(){return _9y(_a1,_a2,_a3,_a5);});}}},_ab=function(_ac){var _ad=E(_ac);if(!_ad._){return new T0(1);}else{var _ae=E(_ad.a),_af=_ae.a,_ag=_ae.b,_ah=E(_ad.b);if(!_ah._){var _ai=E(_af);return new T5(0,1,E(_ai),_ag,E(_6t),E(_6t));}else{var _aj=_ah.b,_ak=E(_ah.a),_al=_ak.b,_am=E(_af),_an=E(_ak.a);if(_am<_an){return new F(function(){return _9Z(1,new T5(0,1,E(_am),_ag,E(_6t),E(_6t)),_an,_al,_aj);});}else{return new F(function(){return _9y(new T5(0,1,E(_am),_ag,E(_6t),E(_6t)),_an,_al,_aj);});}}}},_ao=function(_ap){var _aq=new T(function(){return new T2(1,_ap,_aq);});return E(_aq);},_ar=function(_as,_at){var _au=E(_at);if(!_au._){return new F(function(){return _ao(_as);});}else{return new T2(1,_as,new T(function(){return B(_ar(_au.a,_au.b));}));}},_av=function(_aw){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_3e(9,_aw,_u));}))));});},_ax=function(_ay,_az){var _aA=_ay%_az;if(_ay<=0){if(_ay>=0){return E(_aA);}else{if(_az<=0){return E(_aA);}else{var _aB=E(_aA);return (_aB==0)?0:_aB+_az|0;}}}else{if(_az>=0){if(_ay>=0){return E(_aA);}else{if(_az<=0){return E(_aA);}else{var _aC=E(_aA);return (_aC==0)?0:_aC+_az|0;}}}else{var _aD=E(_aA);return (_aD==0)?0:_aD+_az|0;}}},_aE=function(_aF,_aG){var _aH=function(_aI){if(65>_aG){return E(_aG);}else{if(_aG>90){return E(_aG);}else{var _aJ=B(_ax(B(A1(_aF,_aG-64|0)),26));if(!_aJ){return 90;}else{var _aK=64+_aJ|0;if(_aK>>>0>1114111){return new F(function(){return _av(_aK);});}else{return _aK;}}}}};if(97>_aG){return new F(function(){return _aH(_);});}else{if(_aG>122){return new F(function(){return _aH(_);});}else{var _aL=B(_ax(B(A1(_aF,_aG-96|0)),26));if(!_aL){return 122;}else{var _aM=96+_aL|0;if(_aM>>>0>1114111){return new F(function(){return _av(_aM);});}else{return _aM;}}}}},_aN=function(_aO,_aP){while(1){var _aQ=E(_aP);if(!_aQ._){var _aR=E(_aQ.b);if(_aO!=_aR){if(_aO>_aR){_aP=_aQ.e;continue;}else{_aP=_aQ.d;continue;}}else{return new T1(1,_aQ.c);}}else{return __Z;}}},_aS=2,_aT=new T1(1,_aS),_aU=new T(function(){return B(unCStr("oct"));}),_aV=new T(function(){return B(unCStr("numbers"));}),_aW=new T(function(){return B(unCStr("number"));}),_aX=new T(function(){return B(unCStr("hexadecimal"));}),_aY=new T(function(){return B(unCStr("hex"));}),_aZ=new T(function(){return B(unCStr("decimal"));}),_b0=new T(function(){return B(unCStr("binary"));}),_b1=new T(function(){return B(unCStr("bin"));}),_b2=16,_b3=new T1(1,_b2),_b4=10,_b5=new T1(1,_b4),_b6=8,_b7=new T1(1,_b6),_b8=new T(function(){return B(unCStr("octal"));}),_b9=function(_ba,_bb){while(1){var _bc=E(_ba);if(!_bc._){return (E(_bb)._==0)?true:false;}else{var _bd=E(_bb);if(!_bd._){return false;}else{if(E(_bc.a)!=E(_bd.a)){return false;}else{_ba=_bc.b;_bb=_bd.b;continue;}}}}},_be=function(_bf){return (!B(_b9(_bf,_b1)))?(!B(_b9(_bf,_b0)))?(!B(_b9(_bf,_aZ)))?(!B(_b9(_bf,_aY)))?(!B(_b9(_bf,_aX)))?(!B(_b9(_bf,_aW)))?(!B(_b9(_bf,_aV)))?(!B(_b9(_bf,_aU)))?(!B(_b9(_bf,_b8)))?__Z:E(_b7):E(_b7):E(_b5):E(_b5):E(_b3):E(_b3):E(_b5):E(_aT):E(_aT);},_bg=function(_bh){var _bi=u_towlower(E(_bh));if(_bi>>>0>1114111){return new F(function(){return _av(_bi);});}else{return _bi;}},_bj=new T1(1,_bg),_bk=function(_bl){var _bm=u_towupper(E(_bl));if(_bm>>>0>1114111){return new F(function(){return _av(_bm);});}else{return _bm;}},_bn=new T1(1,_bk),_bo=new T(function(){return B(unCStr("uppercased"));}),_bp=new T(function(){return B(unCStr("uppercase"));}),_bq=new T(function(){return B(unCStr("upper"));}),_br=new T(function(){return B(unCStr("lowercased"));}),_bs=new T(function(){return B(unCStr("lowercase"));}),_bt=new T(function(){return B(unCStr("lower"));}),_bu=function(_bv){return (!B(_b9(_bv,_bt)))?(!B(_b9(_bv,_bs)))?(!B(_b9(_bv,_br)))?(!B(_b9(_bv,_bq)))?(!B(_b9(_bv,_bp)))?(!B(_b9(_bv,_bo)))?__Z:E(_bn):E(_bn):E(_bn):E(_bj):E(_bj):E(_bj);},_bw=function(_bx,_by){return E(_bx)!=E(_by);},_bz=function(_bA,_bB){return E(_bA)==E(_bB);},_bC=new T2(0,_bz,_bw),_bD=function(_bE){return E(E(_bE).a);},_bF=function(_bG,_bH,_bI){while(1){var _bJ=E(_bI);if(!_bJ._){return false;}else{if(!B(A3(_bD,_bG,_bH,_bJ.a))){_bI=_bJ.b;continue;}else{return true;}}}},_bK=function(_bL,_bM){while(1){var _bN=E(_bL);if(!_bN._){return E(_bM);}else{_bL=_bN.b;_bM=_bN.a;continue;}}},_bO=function(_bP,_bQ){var _bR=E(_bQ);return (_bR._==0)?__Z:new T2(1,_bP,new T(function(){return B(_bO(_bR.a,_bR.b));}));},_bS=new T(function(){return B(unCStr(": empty list"));}),_bT=new T(function(){return B(unCStr("Prelude."));}),_bU=function(_bV){return new F(function(){return err(B(_O(_bT,new T(function(){return B(_O(_bV,_bS));},1))));});},_bW=new T(function(){return B(unCStr("init"));}),_bX=new T(function(){return B(_bU(_bW));}),_bY=new T(function(){return B(unCStr("last"));}),_bZ=new T(function(){return B(_bU(_bY));}),_c0=function(_c1){var _c2=_c1>>>0;if(_c2>887){var _c3=u_iswspace(_c1);return (E(_c3)==0)?false:true;}else{var _c4=E(_c2);return (_c4==32)?true:(_c4-9>>>0>4)?(E(_c4)==160)?true:false:true;}},_c5=function(_c6){return new F(function(){return _c0(E(_c6));});},_c7=new T1(1,_c5),_c8=new T(function(){return B(unCStr("whitespace"));}),_c9=new T(function(){return B(unCStr(")"));}),_ca=new T(function(){return B(_3e(0,29,_c9));}),_cb=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_ca));}),_cc=function(_cd){return new F(function(){return err(B(unAppCStr("toEnum{GeneralCategory}: tag (",new T(function(){return B(_3e(0,_cd,_cb));}))));});},_ce=function(_cf){var _cg=u_gencat(_cf);if(_cg<0){return new F(function(){return _cc(_cg);});}else{if(_cg>29){return new F(function(){return _cc(_cg);});}else{return _cg;}}},_ch=function(_ci){switch(B(_ce(_ci))){case 11:return true;case 12:return true;case 13:return true;case 14:return true;case 15:return true;case 16:return true;case 17:return true;default:return false;}},_cj=function(_ck){return new F(function(){return _ch(E(_ck));});},_cl=new T1(1,_cj),_cm=new T(function(){return B(unCStr("punctuation"));}),_cn=function(_co){return (E(_co)-48|0)>>>0<=9;},_cp=new T1(1,_cn),_cq=function(_cr){switch(B(_ce(E(_cr)))){case 0:return true;case 1:return true;case 2:return true;case 3:return true;case 4:return true;default:return false;}},_cs=new T1(1,_cq),_ct=new T(function(){return B(unCStr("letters"));}),_cu=new T(function(){return B(unCStr("letter"));}),_cv=new T(function(){return B(unCStr("digits"));}),_cw=function(_cx){var _cy=function(_cz){switch(E(_cx)){case 65:return false;case 69:return false;case 73:return false;case 79:return false;case 85:return false;case 97:return false;case 101:return false;case 105:return false;case 111:return false;case 117:return false;default:return true;}};switch(B(_ce(_cx))){case 0:return new F(function(){return _cy(_);});break;case 1:return new F(function(){return _cy(_);});break;case 2:return new F(function(){return _cy(_);});break;case 3:return new F(function(){return _cy(_);});break;case 4:return new F(function(){return _cy(_);});break;default:return false;}},_cA=function(_cB){return new F(function(){return _cw(E(_cB));});},_cC=new T1(1,_cA),_cD=new T(function(){return B(unCStr("consonants"));}),_cE=new T(function(){return B(unCStr("consonant"));}),_cF=function(_cG){switch(E(_cG)){case 65:return true;case 69:return true;case 73:return true;case 79:return true;case 85:return true;case 97:return true;case 101:return true;case 105:return true;case 111:return true;case 117:return true;default:return false;}},_cH=function(_cI){return new F(function(){return _cF(E(_cI));});},_cJ=new T1(1,_cH),_cK=function(_cL){var _cM=u_iswalpha(E(_cL));return (E(_cM)==0)?false:true;},_cN=new T1(1,_cK),_cO=new T(function(){return B(unCStr("alpha"));}),_cP=new T(function(){return B(unCStr("vowels"));}),_cQ=new T(function(){return B(unCStr("vowel"));}),_cR=function(_cS){switch(B(_ce(E(_cS)))){case 18:return true;case 19:return true;case 20:return true;case 21:return true;default:return false;}},_cT=new T1(1,_cR),_cU=new T(function(){return B(unCStr("symbols"));}),_cV=new T(function(){return B(unCStr("symbol"));}),_cW=new T(function(){return B(unCStr("spaces"));}),_cX=new T(function(){return B(unCStr("space"));}),_cY=new T(function(){return B(unCStr("digit"));}),_cZ=function(_d0){if(!B(_b9(_d0,_cO))){if(!B(_b9(_d0,_cE))){if(!B(_b9(_d0,_cD))){if(!B(_b9(_d0,_cY))){if(!B(_b9(_d0,_cv))){if(!B(_b9(_d0,_cu))){if(!B(_b9(_d0,_ct))){if(!B(_b9(_d0,_aW))){if(!B(_b9(_d0,_aV))){if(!B(_b9(_d0,_cm))){if(!B(_b9(_d0,_cX))){if(!B(_b9(_d0,_cW))){if(!B(_b9(_d0,_cV))){if(!B(_b9(_d0,_cU))){if(!B(_b9(_d0,_cQ))){if(!B(_b9(_d0,_cP))){if(!B(_b9(_d0,_c8))){var _d1=E(_d0);if(!_d1._){return __Z;}else{var _d2=_d1.b;if(E(_d1.a)==91){if(E(B(_bK(_d2,_bZ)))==93){var _d3=new T(function(){var _d4=E(_d2);if(!_d4._){return E(_bX);}else{return B(_bO(_d4.a,_d4.b));}});return new T1(1,function(_d5){return new F(function(){return _bF(_bC,_d5,_d3);});});}else{return __Z;}}else{return __Z;}}}else{return E(_c7);}}else{return E(_cJ);}}else{return E(_cJ);}}else{return E(_cT);}}else{return E(_cT);}}else{return E(_c7);}}else{return E(_c7);}}else{return E(_cl);}}else{return E(_cp);}}else{return E(_cp);}}else{return E(_cs);}}else{return E(_cs);}}else{return E(_cp);}}else{return E(_cp);}}else{return E(_cC);}}else{return E(_cC);}}else{return E(_cN);}},_d6=function(_d7,_d8){var _d9=function(_da){var _db=E(_da);if(!_db._){return __Z;}else{var _dc=_db.b,_dd=E(_db.a);if(!_dd._){return new T2(1,new T1(0,_dd.a),new T(function(){return B(_d9(_dc));}));}else{return new F(function(){return _O(B(A1(_d7,_dd.a)),new T(function(){return B(_d9(_dc));},1));});}}};return new F(function(){return _d9(_d8);});},_de=function(_df){var _dg=E(_df);if(!_dg._){return __Z;}else{return new F(function(){return _O(_dg.a,new T(function(){return B(_de(_dg.b));},1));});}},_dh=function(_di){return new F(function(){return _de(_di);});},_dj=function(_dk){var _dl=E(_dk);return (_dl._==0)?new T1(0,_dl.a):new T1(1,new T(function(){return B(_dh(_dl.a));}));},_dm=function(_dn){var _do=E(_dn);if(!_do._){return __Z;}else{var _dp=_do.b,_dq=E(_do.a);if(!_dq._){return new T2(1,new T1(0,_dq.a),new T(function(){return B(_dm(_dp));}));}else{var _dr=_dq.a,_ds=B(_dm(_dp));if(!_ds._){return new T2(1,new T1(1,new T2(1,_dr,_u)),_u);}else{var _dt=E(_ds.a);return (_dt._==0)?new T2(1,new T1(1,new T2(1,_dr,_u)),_ds):new T2(1,new T1(1,new T2(1,_dr,_dt.a)),_ds.b);}}}},_du=function(_dv,_dw){var _dx=E(_dw);return (_dx._==0)?__Z:new T2(1,new T(function(){return B(A1(_dv,_dx.a));}),new T(function(){return B(_du(_dv,_dx.b));}));},_dy=function(_dz){return new F(function(){return _du(_dj,B(_dm(_dz)));});},_dA=34,_dB=new T2(1,_dA,_u),_dC=new T(function(){return B(_5z("Text/ParserCombinators/ReadP.hs:(128,3)-(151,52)|function <|>"));}),_dD=function(_dE,_dF){while(1){var _dG=B((function(_dH,_dI){var _dJ=E(_dH);switch(_dJ._){case 0:var _dK=E(_dI);if(!_dK._){return __Z;}else{_dE=B(A1(_dJ.a,_dK.a));_dF=_dK.b;return __continue;}break;case 1:var _dL=B(A1(_dJ.a,_dI)),_dM=_dI;_dE=_dL;_dF=_dM;return __continue;case 2:return __Z;case 3:return new T2(1,new T2(0,_dJ.a,_dI),new T(function(){return B(_dD(_dJ.b,_dI));}));default:return E(_dJ.a);}})(_dE,_dF));if(_dG!=__continue){return _dG;}}},_dN=function(_dO,_dP){var _dQ=function(_dR){var _dS=E(_dP);if(_dS._==3){return new T2(3,_dS.a,new T(function(){return B(_dN(_dO,_dS.b));}));}else{var _dT=E(_dO);if(_dT._==2){return E(_dS);}else{var _dU=E(_dS);if(_dU._==2){return E(_dT);}else{var _dV=function(_dW){var _dX=E(_dU);if(_dX._==4){var _dY=function(_dZ){return new T1(4,new T(function(){return B(_O(B(_dD(_dT,_dZ)),_dX.a));}));};return new T1(1,_dY);}else{var _e0=E(_dT);if(_e0._==1){var _e1=_e0.a,_e2=E(_dX);if(!_e2._){return new T1(1,function(_e3){return new F(function(){return _dN(B(A1(_e1,_e3)),_e2);});});}else{var _e4=function(_e5){return new F(function(){return _dN(B(A1(_e1,_e5)),new T(function(){return B(A1(_e2.a,_e5));}));});};return new T1(1,_e4);}}else{var _e6=E(_dX);if(!_e6._){return E(_dC);}else{var _e7=function(_e8){return new F(function(){return _dN(_e0,new T(function(){return B(A1(_e6.a,_e8));}));});};return new T1(1,_e7);}}}},_e9=E(_dT);switch(_e9._){case 1:var _ea=E(_dU);if(_ea._==4){var _eb=function(_ec){return new T1(4,new T(function(){return B(_O(B(_dD(B(A1(_e9.a,_ec)),_ec)),_ea.a));}));};return new T1(1,_eb);}else{return new F(function(){return _dV(_);});}break;case 4:var _ed=_e9.a,_ee=E(_dU);switch(_ee._){case 0:var _ef=function(_eg){var _eh=new T(function(){return B(_O(_ed,new T(function(){return B(_dD(_ee,_eg));},1)));});return new T1(4,_eh);};return new T1(1,_ef);case 1:var _ei=function(_ej){var _ek=new T(function(){return B(_O(_ed,new T(function(){return B(_dD(B(A1(_ee.a,_ej)),_ej));},1)));});return new T1(4,_ek);};return new T1(1,_ei);default:return new T1(4,new T(function(){return B(_O(_ed,_ee.a));}));}break;default:return new F(function(){return _dV(_);});}}}}},_el=E(_dO);switch(_el._){case 0:var _em=E(_dP);if(!_em._){var _en=function(_eo){return new F(function(){return _dN(B(A1(_el.a,_eo)),new T(function(){return B(A1(_em.a,_eo));}));});};return new T1(0,_en);}else{return new F(function(){return _dQ(_);});}break;case 3:return new T2(3,_el.a,new T(function(){return B(_dN(_el.b,_dP));}));default:return new F(function(){return _dQ(_);});}},_ep=new T(function(){return B(unCStr("("));}),_eq=new T(function(){return B(unCStr(")"));}),_er=function(_es,_et){while(1){var _eu=E(_es);if(!_eu._){return (E(_et)._==0)?true:false;}else{var _ev=E(_et);if(!_ev._){return false;}else{if(E(_eu.a)!=E(_ev.a)){return false;}else{_es=_eu.b;_et=_ev.b;continue;}}}}},_ew=function(_ex,_ey){return (!B(_er(_ex,_ey)))?true:false;},_ez=new T2(0,_er,_ew),_eA=function(_eB,_eC){var _eD=E(_eB);switch(_eD._){case 0:return new T1(0,function(_eE){return new F(function(){return _eA(B(A1(_eD.a,_eE)),_eC);});});case 1:return new T1(1,function(_eF){return new F(function(){return _eA(B(A1(_eD.a,_eF)),_eC);});});case 2:return new T0(2);case 3:return new F(function(){return _dN(B(A1(_eC,_eD.a)),new T(function(){return B(_eA(_eD.b,_eC));}));});break;default:var _eG=function(_eH){var _eI=E(_eH);if(!_eI._){return __Z;}else{var _eJ=E(_eI.a);return new F(function(){return _O(B(_dD(B(A1(_eC,_eJ.a)),_eJ.b)),new T(function(){return B(_eG(_eI.b));},1));});}},_eK=B(_eG(_eD.a));return (_eK._==0)?new T0(2):new T1(4,_eK);}},_eL=new T0(2),_eM=function(_eN){return new T2(3,_eN,_eL);},_eO=function(_eP,_eQ){var _eR=E(_eP);if(!_eR){return new F(function(){return A1(_eQ,_2t);});}else{var _eS=new T(function(){return B(_eO(_eR-1|0,_eQ));});return new T1(0,function(_eT){return E(_eS);});}},_eU=function(_eV,_eW,_eX){var _eY=new T(function(){return B(A1(_eV,_eM));}),_eZ=function(_f0,_f1,_f2,_f3){while(1){var _f4=B((function(_f5,_f6,_f7,_f8){var _f9=E(_f5);switch(_f9._){case 0:var _fa=E(_f6);if(!_fa._){return new F(function(){return A1(_eW,_f8);});}else{var _fb=_f7+1|0,_fc=_f8;_f0=B(A1(_f9.a,_fa.a));_f1=_fa.b;_f2=_fb;_f3=_fc;return __continue;}break;case 1:var _fd=B(A1(_f9.a,_f6)),_fe=_f6,_fb=_f7,_fc=_f8;_f0=_fd;_f1=_fe;_f2=_fb;_f3=_fc;return __continue;case 2:return new F(function(){return A1(_eW,_f8);});break;case 3:var _ff=new T(function(){return B(_eA(_f9,_f8));});return new F(function(){return _eO(_f7,function(_fg){return E(_ff);});});break;default:return new F(function(){return _eA(_f9,_f8);});}})(_f0,_f1,_f2,_f3));if(_f4!=__continue){return _f4;}}};return function(_fh){return new F(function(){return _eZ(_eY,_fh,0,_eX);});};},_fi=function(_fj){return new F(function(){return A1(_fj,_u);});},_fk=function(_fl,_fm){var _fn=function(_fo){var _fp=E(_fo);if(!_fp._){return E(_fi);}else{var _fq=_fp.a;if(!B(A1(_fl,_fq))){return E(_fi);}else{var _fr=new T(function(){return B(_fn(_fp.b));}),_fs=function(_ft){var _fu=new T(function(){return B(A1(_fr,function(_fv){return new F(function(){return A1(_ft,new T2(1,_fq,_fv));});}));});return new T1(0,function(_fw){return E(_fu);});};return E(_fs);}}};return function(_fx){return new F(function(){return A2(_fn,_fx,_fm);});};},_fy=new T0(6),_fz=new T(function(){return B(unCStr("valDig: Bad base"));}),_fA=new T(function(){return B(err(_fz));}),_fB=function(_fC,_fD){var _fE=function(_fF,_fG){var _fH=E(_fF);if(!_fH._){var _fI=new T(function(){return B(A1(_fG,_u));});return function(_fJ){return new F(function(){return A1(_fJ,_fI);});};}else{var _fK=E(_fH.a),_fL=function(_fM){var _fN=new T(function(){return B(_fE(_fH.b,function(_fO){return new F(function(){return A1(_fG,new T2(1,_fM,_fO));});}));}),_fP=function(_fQ){var _fR=new T(function(){return B(A1(_fN,_fQ));});return new T1(0,function(_fS){return E(_fR);});};return E(_fP);};switch(E(_fC)){case 8:if(48>_fK){var _fT=new T(function(){return B(A1(_fG,_u));});return function(_fU){return new F(function(){return A1(_fU,_fT);});};}else{if(_fK>55){var _fV=new T(function(){return B(A1(_fG,_u));});return function(_fW){return new F(function(){return A1(_fW,_fV);});};}else{return new F(function(){return _fL(_fK-48|0);});}}break;case 10:if(48>_fK){var _fX=new T(function(){return B(A1(_fG,_u));});return function(_fY){return new F(function(){return A1(_fY,_fX);});};}else{if(_fK>57){var _fZ=new T(function(){return B(A1(_fG,_u));});return function(_g0){return new F(function(){return A1(_g0,_fZ);});};}else{return new F(function(){return _fL(_fK-48|0);});}}break;case 16:if(48>_fK){if(97>_fK){if(65>_fK){var _g1=new T(function(){return B(A1(_fG,_u));});return function(_g2){return new F(function(){return A1(_g2,_g1);});};}else{if(_fK>70){var _g3=new T(function(){return B(A1(_fG,_u));});return function(_g4){return new F(function(){return A1(_g4,_g3);});};}else{return new F(function(){return _fL((_fK-65|0)+10|0);});}}}else{if(_fK>102){if(65>_fK){var _g5=new T(function(){return B(A1(_fG,_u));});return function(_g6){return new F(function(){return A1(_g6,_g5);});};}else{if(_fK>70){var _g7=new T(function(){return B(A1(_fG,_u));});return function(_g8){return new F(function(){return A1(_g8,_g7);});};}else{return new F(function(){return _fL((_fK-65|0)+10|0);});}}}else{return new F(function(){return _fL((_fK-97|0)+10|0);});}}}else{if(_fK>57){if(97>_fK){if(65>_fK){var _g9=new T(function(){return B(A1(_fG,_u));});return function(_ga){return new F(function(){return A1(_ga,_g9);});};}else{if(_fK>70){var _gb=new T(function(){return B(A1(_fG,_u));});return function(_gc){return new F(function(){return A1(_gc,_gb);});};}else{return new F(function(){return _fL((_fK-65|0)+10|0);});}}}else{if(_fK>102){if(65>_fK){var _gd=new T(function(){return B(A1(_fG,_u));});return function(_ge){return new F(function(){return A1(_ge,_gd);});};}else{if(_fK>70){var _gf=new T(function(){return B(A1(_fG,_u));});return function(_gg){return new F(function(){return A1(_gg,_gf);});};}else{return new F(function(){return _fL((_fK-65|0)+10|0);});}}}else{return new F(function(){return _fL((_fK-97|0)+10|0);});}}}else{return new F(function(){return _fL(_fK-48|0);});}}break;default:return E(_fA);}}},_gh=function(_gi){var _gj=E(_gi);if(!_gj._){return new T0(2);}else{return new F(function(){return A1(_fD,_gj);});}};return function(_gk){return new F(function(){return A3(_fE,_gk,_2q,_gh);});};},_gl=16,_gm=8,_gn=function(_go){var _gp=function(_gq){return new F(function(){return A1(_go,new T1(5,new T2(0,_gm,_gq)));});},_gr=function(_gs){return new F(function(){return A1(_go,new T1(5,new T2(0,_gl,_gs)));});},_gt=function(_gu){switch(E(_gu)){case 79:return new T1(1,B(_fB(_gm,_gp)));case 88:return new T1(1,B(_fB(_gl,_gr)));case 111:return new T1(1,B(_fB(_gm,_gp)));case 120:return new T1(1,B(_fB(_gl,_gr)));default:return new T0(2);}};return function(_gv){return (E(_gv)==48)?E(new T1(0,_gt)):new T0(2);};},_gw=function(_gx){return new T1(0,B(_gn(_gx)));},_gy=function(_gz){return new F(function(){return A1(_gz,_2a);});},_gA=function(_gB){return new F(function(){return A1(_gB,_2a);});},_gC=10,_gD=new T1(0,1),_gE=new T1(0,2147483647),_gF=function(_gG,_gH){while(1){var _gI=E(_gG);if(!_gI._){var _gJ=_gI.a,_gK=E(_gH);if(!_gK._){var _gL=_gK.a,_gM=addC(_gJ,_gL);if(!E(_gM.b)){return new T1(0,_gM.a);}else{_gG=new T1(1,I_fromInt(_gJ));_gH=new T1(1,I_fromInt(_gL));continue;}}else{_gG=new T1(1,I_fromInt(_gJ));_gH=_gK;continue;}}else{var _gN=E(_gH);if(!_gN._){_gG=_gI;_gH=new T1(1,I_fromInt(_gN.a));continue;}else{return new T1(1,I_add(_gI.a,_gN.a));}}}},_gO=new T(function(){return B(_gF(_gE,_gD));}),_gP=function(_gQ){var _gR=E(_gQ);if(!_gR._){var _gS=E(_gR.a);return (_gS==(-2147483648))?E(_gO):new T1(0, -_gS);}else{return new T1(1,I_negate(_gR.a));}},_gT=new T1(0,10),_gU=function(_gV,_gW){while(1){var _gX=E(_gV);if(!_gX._){return E(_gW);}else{var _gY=_gW+1|0;_gV=_gX.b;_gW=_gY;continue;}}},_gZ=function(_h0){return new T1(0,_h0);},_h1=function(_h2){return new F(function(){return _gZ(E(_h2));});},_h3=new T(function(){return B(unCStr("this should not happen"));}),_h4=new T(function(){return B(err(_h3));}),_h5=function(_h6,_h7){while(1){var _h8=E(_h6);if(!_h8._){var _h9=_h8.a,_ha=E(_h7);if(!_ha._){var _hb=_ha.a;if(!(imul(_h9,_hb)|0)){return new T1(0,imul(_h9,_hb)|0);}else{_h6=new T1(1,I_fromInt(_h9));_h7=new T1(1,I_fromInt(_hb));continue;}}else{_h6=new T1(1,I_fromInt(_h9));_h7=_ha;continue;}}else{var _hc=E(_h7);if(!_hc._){_h6=_h8;_h7=new T1(1,I_fromInt(_hc.a));continue;}else{return new T1(1,I_mul(_h8.a,_hc.a));}}}},_hd=function(_he,_hf){var _hg=E(_hf);if(!_hg._){return __Z;}else{var _hh=E(_hg.b);return (_hh._==0)?E(_h4):new T2(1,B(_gF(B(_h5(_hg.a,_he)),_hh.a)),new T(function(){return B(_hd(_he,_hh.b));}));}},_hi=new T1(0,0),_hj=function(_hk,_hl,_hm){while(1){var _hn=B((function(_ho,_hp,_hq){var _hr=E(_hq);if(!_hr._){return E(_hi);}else{if(!E(_hr.b)._){return E(_hr.a);}else{var _hs=E(_hp);if(_hs<=40){var _ht=function(_hu,_hv){while(1){var _hw=E(_hv);if(!_hw._){return E(_hu);}else{var _hx=B(_gF(B(_h5(_hu,_ho)),_hw.a));_hu=_hx;_hv=_hw.b;continue;}}};return new F(function(){return _ht(_hi,_hr);});}else{var _hy=B(_h5(_ho,_ho));if(!(_hs%2)){var _hz=B(_hd(_ho,_hr));_hk=_hy;_hl=quot(_hs+1|0,2);_hm=_hz;return __continue;}else{var _hz=B(_hd(_ho,new T2(1,_hi,_hr)));_hk=_hy;_hl=quot(_hs+1|0,2);_hm=_hz;return __continue;}}}}})(_hk,_hl,_hm));if(_hn!=__continue){return _hn;}}},_hA=function(_hB,_hC){return new F(function(){return _hj(_hB,new T(function(){return B(_gU(_hC,0));},1),B(_du(_h1,_hC)));});},_hD=function(_hE){var _hF=new T(function(){var _hG=new T(function(){var _hH=function(_hI){return new F(function(){return A1(_hE,new T1(1,new T(function(){return B(_hA(_gT,_hI));})));});};return new T1(1,B(_fB(_gC,_hH)));}),_hJ=function(_hK){if(E(_hK)==43){var _hL=function(_hM){return new F(function(){return A1(_hE,new T1(1,new T(function(){return B(_hA(_gT,_hM));})));});};return new T1(1,B(_fB(_gC,_hL)));}else{return new T0(2);}},_hN=function(_hO){if(E(_hO)==45){var _hP=function(_hQ){return new F(function(){return A1(_hE,new T1(1,new T(function(){return B(_gP(B(_hA(_gT,_hQ))));})));});};return new T1(1,B(_fB(_gC,_hP)));}else{return new T0(2);}};return B(_dN(B(_dN(new T1(0,_hN),new T1(0,_hJ))),_hG));});return new F(function(){return _dN(new T1(0,function(_hR){return (E(_hR)==101)?E(_hF):new T0(2);}),new T1(0,function(_hS){return (E(_hS)==69)?E(_hF):new T0(2);}));});},_hT=function(_hU){var _hV=function(_hW){return new F(function(){return A1(_hU,new T1(1,_hW));});};return function(_hX){return (E(_hX)==46)?new T1(1,B(_fB(_gC,_hV))):new T0(2);};},_hY=function(_hZ){return new T1(0,B(_hT(_hZ)));},_i0=function(_i1){var _i2=function(_i3){var _i4=function(_i5){return new T1(1,B(_eU(_hD,_gy,function(_i6){return new F(function(){return A1(_i1,new T1(5,new T3(1,_i3,_i5,_i6)));});})));};return new T1(1,B(_eU(_hY,_gA,_i4)));};return new F(function(){return _fB(_gC,_i2);});},_i7=function(_i8){return new T1(1,B(_i0(_i8)));},_i9=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_ia=function(_ib){return new F(function(){return _bF(_bC,_ib,_i9);});},_ic=false,_id=true,_ie=function(_if){var _ig=new T(function(){return B(A1(_if,_gm));}),_ih=new T(function(){return B(A1(_if,_gl));});return function(_ii){switch(E(_ii)){case 79:return E(_ig);case 88:return E(_ih);case 111:return E(_ig);case 120:return E(_ih);default:return new T0(2);}};},_ij=function(_ik){return new T1(0,B(_ie(_ik)));},_il=function(_im){return new F(function(){return A1(_im,_gC);});},_in=function(_io){var _ip=E(_io);if(!_ip._){return E(_ip.a);}else{return new F(function(){return I_toInt(_ip.a);});}},_iq=function(_ir,_is){var _it=E(_ir);if(!_it._){var _iu=_it.a,_iv=E(_is);return (_iv._==0)?_iu<=_iv.a:I_compareInt(_iv.a,_iu)>=0;}else{var _iw=_it.a,_ix=E(_is);return (_ix._==0)?I_compareInt(_iw,_ix.a)<=0:I_compare(_iw,_ix.a)<=0;}},_iy=function(_iz){return new T0(2);},_iA=function(_iB){var _iC=E(_iB);if(!_iC._){return E(_iy);}else{var _iD=_iC.a,_iE=E(_iC.b);if(!_iE._){return E(_iD);}else{var _iF=new T(function(){return B(_iA(_iE));}),_iG=function(_iH){return new F(function(){return _dN(B(A1(_iD,_iH)),new T(function(){return B(A1(_iF,_iH));}));});};return E(_iG);}}},_iI=function(_iJ,_iK){var _iL=function(_iM,_iN,_iO){var _iP=E(_iM);if(!_iP._){return new F(function(){return A1(_iO,_iJ);});}else{var _iQ=E(_iN);if(!_iQ._){return new T0(2);}else{if(E(_iP.a)!=E(_iQ.a)){return new T0(2);}else{var _iR=new T(function(){return B(_iL(_iP.b,_iQ.b,_iO));});return new T1(0,function(_iS){return E(_iR);});}}}};return function(_iT){return new F(function(){return _iL(_iJ,_iT,_iK);});};},_iU=new T(function(){return B(unCStr("SO"));}),_iV=14,_iW=function(_iX){var _iY=new T(function(){return B(A1(_iX,_iV));});return new T1(1,B(_iI(_iU,function(_iZ){return E(_iY);})));},_j0=new T(function(){return B(unCStr("SOH"));}),_j1=1,_j2=function(_j3){var _j4=new T(function(){return B(A1(_j3,_j1));});return new T1(1,B(_iI(_j0,function(_j5){return E(_j4);})));},_j6=function(_j7){return new T1(1,B(_eU(_j2,_iW,_j7)));},_j8=new T(function(){return B(unCStr("NUL"));}),_j9=0,_ja=function(_jb){var _jc=new T(function(){return B(A1(_jb,_j9));});return new T1(1,B(_iI(_j8,function(_jd){return E(_jc);})));},_je=new T(function(){return B(unCStr("STX"));}),_jf=2,_jg=function(_jh){var _ji=new T(function(){return B(A1(_jh,_jf));});return new T1(1,B(_iI(_je,function(_jj){return E(_ji);})));},_jk=new T(function(){return B(unCStr("ETX"));}),_jl=3,_jm=function(_jn){var _jo=new T(function(){return B(A1(_jn,_jl));});return new T1(1,B(_iI(_jk,function(_jp){return E(_jo);})));},_jq=new T(function(){return B(unCStr("EOT"));}),_jr=4,_js=function(_jt){var _ju=new T(function(){return B(A1(_jt,_jr));});return new T1(1,B(_iI(_jq,function(_jv){return E(_ju);})));},_jw=new T(function(){return B(unCStr("ENQ"));}),_jx=5,_jy=function(_jz){var _jA=new T(function(){return B(A1(_jz,_jx));});return new T1(1,B(_iI(_jw,function(_jB){return E(_jA);})));},_jC=new T(function(){return B(unCStr("ACK"));}),_jD=6,_jE=function(_jF){var _jG=new T(function(){return B(A1(_jF,_jD));});return new T1(1,B(_iI(_jC,function(_jH){return E(_jG);})));},_jI=new T(function(){return B(unCStr("BEL"));}),_jJ=7,_jK=function(_jL){var _jM=new T(function(){return B(A1(_jL,_jJ));});return new T1(1,B(_iI(_jI,function(_jN){return E(_jM);})));},_jO=new T(function(){return B(unCStr("BS"));}),_jP=8,_jQ=function(_jR){var _jS=new T(function(){return B(A1(_jR,_jP));});return new T1(1,B(_iI(_jO,function(_jT){return E(_jS);})));},_jU=new T(function(){return B(unCStr("HT"));}),_jV=9,_jW=function(_jX){var _jY=new T(function(){return B(A1(_jX,_jV));});return new T1(1,B(_iI(_jU,function(_jZ){return E(_jY);})));},_k0=new T(function(){return B(unCStr("LF"));}),_k1=10,_k2=function(_k3){var _k4=new T(function(){return B(A1(_k3,_k1));});return new T1(1,B(_iI(_k0,function(_k5){return E(_k4);})));},_k6=new T(function(){return B(unCStr("VT"));}),_k7=11,_k8=function(_k9){var _ka=new T(function(){return B(A1(_k9,_k7));});return new T1(1,B(_iI(_k6,function(_kb){return E(_ka);})));},_kc=new T(function(){return B(unCStr("FF"));}),_kd=12,_ke=function(_kf){var _kg=new T(function(){return B(A1(_kf,_kd));});return new T1(1,B(_iI(_kc,function(_kh){return E(_kg);})));},_ki=new T(function(){return B(unCStr("CR"));}),_kj=13,_kk=function(_kl){var _km=new T(function(){return B(A1(_kl,_kj));});return new T1(1,B(_iI(_ki,function(_kn){return E(_km);})));},_ko=new T(function(){return B(unCStr("SI"));}),_kp=15,_kq=function(_kr){var _ks=new T(function(){return B(A1(_kr,_kp));});return new T1(1,B(_iI(_ko,function(_kt){return E(_ks);})));},_ku=new T(function(){return B(unCStr("DLE"));}),_kv=16,_kw=function(_kx){var _ky=new T(function(){return B(A1(_kx,_kv));});return new T1(1,B(_iI(_ku,function(_kz){return E(_ky);})));},_kA=new T(function(){return B(unCStr("DC1"));}),_kB=17,_kC=function(_kD){var _kE=new T(function(){return B(A1(_kD,_kB));});return new T1(1,B(_iI(_kA,function(_kF){return E(_kE);})));},_kG=new T(function(){return B(unCStr("DC2"));}),_kH=18,_kI=function(_kJ){var _kK=new T(function(){return B(A1(_kJ,_kH));});return new T1(1,B(_iI(_kG,function(_kL){return E(_kK);})));},_kM=new T(function(){return B(unCStr("DC3"));}),_kN=19,_kO=function(_kP){var _kQ=new T(function(){return B(A1(_kP,_kN));});return new T1(1,B(_iI(_kM,function(_kR){return E(_kQ);})));},_kS=new T(function(){return B(unCStr("DC4"));}),_kT=20,_kU=function(_kV){var _kW=new T(function(){return B(A1(_kV,_kT));});return new T1(1,B(_iI(_kS,function(_kX){return E(_kW);})));},_kY=new T(function(){return B(unCStr("NAK"));}),_kZ=21,_l0=function(_l1){var _l2=new T(function(){return B(A1(_l1,_kZ));});return new T1(1,B(_iI(_kY,function(_l3){return E(_l2);})));},_l4=new T(function(){return B(unCStr("SYN"));}),_l5=22,_l6=function(_l7){var _l8=new T(function(){return B(A1(_l7,_l5));});return new T1(1,B(_iI(_l4,function(_l9){return E(_l8);})));},_la=new T(function(){return B(unCStr("ETB"));}),_lb=23,_lc=function(_ld){var _le=new T(function(){return B(A1(_ld,_lb));});return new T1(1,B(_iI(_la,function(_lf){return E(_le);})));},_lg=new T(function(){return B(unCStr("CAN"));}),_lh=24,_li=function(_lj){var _lk=new T(function(){return B(A1(_lj,_lh));});return new T1(1,B(_iI(_lg,function(_ll){return E(_lk);})));},_lm=new T(function(){return B(unCStr("EM"));}),_ln=25,_lo=function(_lp){var _lq=new T(function(){return B(A1(_lp,_ln));});return new T1(1,B(_iI(_lm,function(_lr){return E(_lq);})));},_ls=new T(function(){return B(unCStr("SUB"));}),_lt=26,_lu=function(_lv){var _lw=new T(function(){return B(A1(_lv,_lt));});return new T1(1,B(_iI(_ls,function(_lx){return E(_lw);})));},_ly=new T(function(){return B(unCStr("ESC"));}),_lz=27,_lA=function(_lB){var _lC=new T(function(){return B(A1(_lB,_lz));});return new T1(1,B(_iI(_ly,function(_lD){return E(_lC);})));},_lE=new T(function(){return B(unCStr("FS"));}),_lF=28,_lG=function(_lH){var _lI=new T(function(){return B(A1(_lH,_lF));});return new T1(1,B(_iI(_lE,function(_lJ){return E(_lI);})));},_lK=new T(function(){return B(unCStr("GS"));}),_lL=29,_lM=function(_lN){var _lO=new T(function(){return B(A1(_lN,_lL));});return new T1(1,B(_iI(_lK,function(_lP){return E(_lO);})));},_lQ=new T(function(){return B(unCStr("RS"));}),_lR=30,_lS=function(_lT){var _lU=new T(function(){return B(A1(_lT,_lR));});return new T1(1,B(_iI(_lQ,function(_lV){return E(_lU);})));},_lW=new T(function(){return B(unCStr("US"));}),_lX=31,_lY=function(_lZ){var _m0=new T(function(){return B(A1(_lZ,_lX));});return new T1(1,B(_iI(_lW,function(_m1){return E(_m0);})));},_m2=new T(function(){return B(unCStr("SP"));}),_m3=32,_m4=function(_m5){var _m6=new T(function(){return B(A1(_m5,_m3));});return new T1(1,B(_iI(_m2,function(_m7){return E(_m6);})));},_m8=new T(function(){return B(unCStr("DEL"));}),_m9=127,_ma=function(_mb){var _mc=new T(function(){return B(A1(_mb,_m9));});return new T1(1,B(_iI(_m8,function(_md){return E(_mc);})));},_me=new T2(1,_ma,_u),_mf=new T2(1,_m4,_me),_mg=new T2(1,_lY,_mf),_mh=new T2(1,_lS,_mg),_mi=new T2(1,_lM,_mh),_mj=new T2(1,_lG,_mi),_mk=new T2(1,_lA,_mj),_ml=new T2(1,_lu,_mk),_mm=new T2(1,_lo,_ml),_mn=new T2(1,_li,_mm),_mo=new T2(1,_lc,_mn),_mp=new T2(1,_l6,_mo),_mq=new T2(1,_l0,_mp),_mr=new T2(1,_kU,_mq),_ms=new T2(1,_kO,_mr),_mt=new T2(1,_kI,_ms),_mu=new T2(1,_kC,_mt),_mv=new T2(1,_kw,_mu),_mw=new T2(1,_kq,_mv),_mx=new T2(1,_kk,_mw),_my=new T2(1,_ke,_mx),_mz=new T2(1,_k8,_my),_mA=new T2(1,_k2,_mz),_mB=new T2(1,_jW,_mA),_mC=new T2(1,_jQ,_mB),_mD=new T2(1,_jK,_mC),_mE=new T2(1,_jE,_mD),_mF=new T2(1,_jy,_mE),_mG=new T2(1,_js,_mF),_mH=new T2(1,_jm,_mG),_mI=new T2(1,_jg,_mH),_mJ=new T2(1,_ja,_mI),_mK=new T2(1,_j6,_mJ),_mL=new T(function(){return B(_iA(_mK));}),_mM=34,_mN=new T1(0,1114111),_mO=92,_mP=39,_mQ=function(_mR){var _mS=new T(function(){return B(A1(_mR,_jJ));}),_mT=new T(function(){return B(A1(_mR,_jP));}),_mU=new T(function(){return B(A1(_mR,_jV));}),_mV=new T(function(){return B(A1(_mR,_k1));}),_mW=new T(function(){return B(A1(_mR,_k7));}),_mX=new T(function(){return B(A1(_mR,_kd));}),_mY=new T(function(){return B(A1(_mR,_kj));}),_mZ=new T(function(){return B(A1(_mR,_mO));}),_n0=new T(function(){return B(A1(_mR,_mP));}),_n1=new T(function(){return B(A1(_mR,_mM));}),_n2=new T(function(){var _n3=function(_n4){var _n5=new T(function(){return B(_gZ(E(_n4)));}),_n6=function(_n7){var _n8=B(_hA(_n5,_n7));if(!B(_iq(_n8,_mN))){return new T0(2);}else{return new F(function(){return A1(_mR,new T(function(){var _n9=B(_in(_n8));if(_n9>>>0>1114111){return B(_av(_n9));}else{return _n9;}}));});}};return new T1(1,B(_fB(_n4,_n6)));},_na=new T(function(){var _nb=new T(function(){return B(A1(_mR,_lX));}),_nc=new T(function(){return B(A1(_mR,_lR));}),_nd=new T(function(){return B(A1(_mR,_lL));}),_ne=new T(function(){return B(A1(_mR,_lF));}),_nf=new T(function(){return B(A1(_mR,_lz));}),_ng=new T(function(){return B(A1(_mR,_lt));}),_nh=new T(function(){return B(A1(_mR,_ln));}),_ni=new T(function(){return B(A1(_mR,_lh));}),_nj=new T(function(){return B(A1(_mR,_lb));}),_nk=new T(function(){return B(A1(_mR,_l5));}),_nl=new T(function(){return B(A1(_mR,_kZ));}),_nm=new T(function(){return B(A1(_mR,_kT));}),_nn=new T(function(){return B(A1(_mR,_kN));}),_no=new T(function(){return B(A1(_mR,_kH));}),_np=new T(function(){return B(A1(_mR,_kB));}),_nq=new T(function(){return B(A1(_mR,_kv));}),_nr=new T(function(){return B(A1(_mR,_kp));}),_ns=new T(function(){return B(A1(_mR,_iV));}),_nt=new T(function(){return B(A1(_mR,_jD));}),_nu=new T(function(){return B(A1(_mR,_jx));}),_nv=new T(function(){return B(A1(_mR,_jr));}),_nw=new T(function(){return B(A1(_mR,_jl));}),_nx=new T(function(){return B(A1(_mR,_jf));}),_ny=new T(function(){return B(A1(_mR,_j1));}),_nz=new T(function(){return B(A1(_mR,_j9));}),_nA=function(_nB){switch(E(_nB)){case 64:return E(_nz);case 65:return E(_ny);case 66:return E(_nx);case 67:return E(_nw);case 68:return E(_nv);case 69:return E(_nu);case 70:return E(_nt);case 71:return E(_mS);case 72:return E(_mT);case 73:return E(_mU);case 74:return E(_mV);case 75:return E(_mW);case 76:return E(_mX);case 77:return E(_mY);case 78:return E(_ns);case 79:return E(_nr);case 80:return E(_nq);case 81:return E(_np);case 82:return E(_no);case 83:return E(_nn);case 84:return E(_nm);case 85:return E(_nl);case 86:return E(_nk);case 87:return E(_nj);case 88:return E(_ni);case 89:return E(_nh);case 90:return E(_ng);case 91:return E(_nf);case 92:return E(_ne);case 93:return E(_nd);case 94:return E(_nc);case 95:return E(_nb);default:return new T0(2);}};return B(_dN(new T1(0,function(_nC){return (E(_nC)==94)?E(new T1(0,_nA)):new T0(2);}),new T(function(){return B(A1(_mL,_mR));})));});return B(_dN(new T1(1,B(_eU(_ij,_il,_n3))),_na));});return new F(function(){return _dN(new T1(0,function(_nD){switch(E(_nD)){case 34:return E(_n1);case 39:return E(_n0);case 92:return E(_mZ);case 97:return E(_mS);case 98:return E(_mT);case 102:return E(_mX);case 110:return E(_mV);case 114:return E(_mY);case 116:return E(_mU);case 118:return E(_mW);default:return new T0(2);}}),_n2);});},_nE=function(_nF){return new F(function(){return A1(_nF,_2t);});},_nG=function(_nH){var _nI=E(_nH);if(!_nI._){return E(_nE);}else{var _nJ=E(_nI.a),_nK=_nJ>>>0,_nL=new T(function(){return B(_nG(_nI.b));});if(_nK>887){var _nM=u_iswspace(_nJ);if(!E(_nM)){return E(_nE);}else{var _nN=function(_nO){var _nP=new T(function(){return B(A1(_nL,_nO));});return new T1(0,function(_nQ){return E(_nP);});};return E(_nN);}}else{var _nR=E(_nK);if(_nR==32){var _nS=function(_nT){var _nU=new T(function(){return B(A1(_nL,_nT));});return new T1(0,function(_nV){return E(_nU);});};return E(_nS);}else{if(_nR-9>>>0>4){if(E(_nR)==160){var _nW=function(_nX){var _nY=new T(function(){return B(A1(_nL,_nX));});return new T1(0,function(_nZ){return E(_nY);});};return E(_nW);}else{return E(_nE);}}else{var _o0=function(_o1){var _o2=new T(function(){return B(A1(_nL,_o1));});return new T1(0,function(_o3){return E(_o2);});};return E(_o0);}}}}},_o4=function(_o5){var _o6=new T(function(){return B(_o4(_o5));}),_o7=function(_o8){return (E(_o8)==92)?E(_o6):new T0(2);},_o9=function(_oa){return E(new T1(0,_o7));},_ob=new T1(1,function(_oc){return new F(function(){return A2(_nG,_oc,_o9);});}),_od=new T(function(){return B(_mQ(function(_oe){return new F(function(){return A1(_o5,new T2(0,_oe,_id));});}));}),_of=function(_og){var _oh=E(_og);if(_oh==38){return E(_o6);}else{var _oi=_oh>>>0;if(_oi>887){var _oj=u_iswspace(_oh);return (E(_oj)==0)?new T0(2):E(_ob);}else{var _ok=E(_oi);return (_ok==32)?E(_ob):(_ok-9>>>0>4)?(E(_ok)==160)?E(_ob):new T0(2):E(_ob);}}};return new F(function(){return _dN(new T1(0,function(_ol){return (E(_ol)==92)?E(new T1(0,_of)):new T0(2);}),new T1(0,function(_om){var _on=E(_om);if(E(_on)==92){return E(_od);}else{return new F(function(){return A1(_o5,new T2(0,_on,_ic));});}}));});},_oo=function(_op,_oq){var _or=new T(function(){return B(A1(_oq,new T1(1,new T(function(){return B(A1(_op,_u));}))));}),_os=function(_ot){var _ou=E(_ot),_ov=E(_ou.a);if(E(_ov)==34){if(!E(_ou.b)){return E(_or);}else{return new F(function(){return _oo(function(_ow){return new F(function(){return A1(_op,new T2(1,_ov,_ow));});},_oq);});}}else{return new F(function(){return _oo(function(_ox){return new F(function(){return A1(_op,new T2(1,_ov,_ox));});},_oq);});}};return new F(function(){return _o4(_os);});},_oy=new T(function(){return B(unCStr("_\'"));}),_oz=function(_oA){var _oB=u_iswalnum(_oA);if(!E(_oB)){return new F(function(){return _bF(_bC,_oA,_oy);});}else{return true;}},_oC=function(_oD){return new F(function(){return _oz(E(_oD));});},_oE=new T(function(){return B(unCStr(",;()[]{}`"));}),_oF=new T(function(){return B(unCStr("=>"));}),_oG=new T2(1,_oF,_u),_oH=new T(function(){return B(unCStr("~"));}),_oI=new T2(1,_oH,_oG),_oJ=new T(function(){return B(unCStr("@"));}),_oK=new T2(1,_oJ,_oI),_oL=new T(function(){return B(unCStr("->"));}),_oM=new T2(1,_oL,_oK),_oN=new T(function(){return B(unCStr("<-"));}),_oO=new T2(1,_oN,_oM),_oP=new T(function(){return B(unCStr("|"));}),_oQ=new T2(1,_oP,_oO),_oR=new T(function(){return B(unCStr("\\"));}),_oS=new T2(1,_oR,_oQ),_oT=new T(function(){return B(unCStr("="));}),_oU=new T2(1,_oT,_oS),_oV=new T(function(){return B(unCStr("::"));}),_oW=new T2(1,_oV,_oU),_oX=new T(function(){return B(unCStr(".."));}),_oY=new T2(1,_oX,_oW),_oZ=function(_p0){var _p1=new T(function(){return B(A1(_p0,_fy));}),_p2=new T(function(){var _p3=new T(function(){var _p4=function(_p5){var _p6=new T(function(){return B(A1(_p0,new T1(0,_p5)));});return new T1(0,function(_p7){return (E(_p7)==39)?E(_p6):new T0(2);});};return B(_mQ(_p4));}),_p8=function(_p9){var _pa=E(_p9);switch(E(_pa)){case 39:return new T0(2);case 92:return E(_p3);default:var _pb=new T(function(){return B(A1(_p0,new T1(0,_pa)));});return new T1(0,function(_pc){return (E(_pc)==39)?E(_pb):new T0(2);});}},_pd=new T(function(){var _pe=new T(function(){return B(_oo(_2q,_p0));}),_pf=new T(function(){var _pg=new T(function(){var _ph=new T(function(){var _pi=function(_pj){var _pk=E(_pj),_pl=u_iswalpha(_pk);return (E(_pl)==0)?(E(_pk)==95)?new T1(1,B(_fk(_oC,function(_pm){return new F(function(){return A1(_p0,new T1(3,new T2(1,_pk,_pm)));});}))):new T0(2):new T1(1,B(_fk(_oC,function(_pn){return new F(function(){return A1(_p0,new T1(3,new T2(1,_pk,_pn)));});})));};return B(_dN(new T1(0,_pi),new T(function(){return new T1(1,B(_eU(_gw,_i7,_p0)));})));}),_po=function(_pp){return (!B(_bF(_bC,_pp,_i9)))?new T0(2):new T1(1,B(_fk(_ia,function(_pq){var _pr=new T2(1,_pp,_pq);if(!B(_bF(_ez,_pr,_oY))){return new F(function(){return A1(_p0,new T1(4,_pr));});}else{return new F(function(){return A1(_p0,new T1(2,_pr));});}})));};return B(_dN(new T1(0,_po),_ph));});return B(_dN(new T1(0,function(_ps){if(!B(_bF(_bC,_ps,_oE))){return new T0(2);}else{return new F(function(){return A1(_p0,new T1(2,new T2(1,_ps,_u)));});}}),_pg));});return B(_dN(new T1(0,function(_pt){return (E(_pt)==34)?E(_pe):new T0(2);}),_pf));});return B(_dN(new T1(0,function(_pu){return (E(_pu)==39)?E(new T1(0,_p8)):new T0(2);}),_pd));});return new F(function(){return _dN(new T1(1,function(_pv){return (E(_pv)._==0)?E(_p1):new T0(2);}),_p2);});},_pw=0,_px=function(_py,_pz){var _pA=new T(function(){var _pB=new T(function(){var _pC=function(_pD){var _pE=new T(function(){var _pF=new T(function(){return B(A1(_pz,_pD));});return B(_oZ(function(_pG){var _pH=E(_pG);return (_pH._==2)?(!B(_b9(_pH.a,_eq)))?new T0(2):E(_pF):new T0(2);}));}),_pI=function(_pJ){return E(_pE);};return new T1(1,function(_pK){return new F(function(){return A2(_nG,_pK,_pI);});});};return B(A2(_py,_pw,_pC));});return B(_oZ(function(_pL){var _pM=E(_pL);return (_pM._==2)?(!B(_b9(_pM.a,_ep)))?new T0(2):E(_pB):new T0(2);}));}),_pN=function(_pO){return E(_pA);};return function(_pP){return new F(function(){return A2(_nG,_pP,_pN);});};},_pQ=function(_pR,_pS){var _pT=function(_pU){var _pV=new T(function(){return B(A1(_pR,_pU));}),_pW=function(_pX){return new F(function(){return _dN(B(A1(_pV,_pX)),new T(function(){return new T1(1,B(_px(_pT,_pX)));}));});};return E(_pW);},_pY=new T(function(){return B(A1(_pR,_pS));}),_pZ=function(_q0){return new F(function(){return _dN(B(A1(_pY,_q0)),new T(function(){return new T1(1,B(_px(_pT,_q0)));}));});};return E(_pZ);},_q1=function(_q2,_q3){var _q4=function(_q5,_q6){var _q7=function(_q8){return new F(function(){return A1(_q6,new T(function(){return  -E(_q8);}));});},_q9=new T(function(){return B(_oZ(function(_qa){return new F(function(){return A3(_q2,_qa,_q5,_q7);});}));}),_qb=function(_qc){return E(_q9);},_qd=function(_qe){return new F(function(){return A2(_nG,_qe,_qb);});},_qf=new T(function(){return B(_oZ(function(_qg){var _qh=E(_qg);if(_qh._==4){var _qi=E(_qh.a);if(!_qi._){return new F(function(){return A3(_q2,_qh,_q5,_q6);});}else{if(E(_qi.a)==45){if(!E(_qi.b)._){return E(new T1(1,_qd));}else{return new F(function(){return A3(_q2,_qh,_q5,_q6);});}}else{return new F(function(){return A3(_q2,_qh,_q5,_q6);});}}}else{return new F(function(){return A3(_q2,_qh,_q5,_q6);});}}));}),_qj=function(_qk){return E(_qf);};return new T1(1,function(_ql){return new F(function(){return A2(_nG,_ql,_qj);});});};return new F(function(){return _pQ(_q4,_q3);});},_qm=function(_qn){var _qo=E(_qn);if(!_qo._){var _qp=_qo.b,_qq=new T(function(){return B(_hj(new T(function(){return B(_gZ(E(_qo.a)));}),new T(function(){return B(_gU(_qp,0));},1),B(_du(_h1,_qp))));});return new T1(1,_qq);}else{return (E(_qo.b)._==0)?(E(_qo.c)._==0)?new T1(1,new T(function(){return B(_hA(_gT,_qo.a));})):__Z:__Z;}},_qr=function(_qs,_qt){return new T0(2);},_qu=function(_qv){var _qw=E(_qv);if(_qw._==5){var _qx=B(_qm(_qw.a));if(!_qx._){return E(_qr);}else{var _qy=new T(function(){return B(_in(_qx.a));});return function(_qz,_qA){return new F(function(){return A1(_qA,_qy);});};}}else{return E(_qr);}},_qB=function(_qC){var _qD=function(_qE){return E(new T2(3,_qC,_eL));};return new T1(1,function(_qF){return new F(function(){return A2(_nG,_qF,_qD);});});},_qG=new T(function(){return B(A3(_q1,_qu,_pw,_qB));}),_qH=function(_qI){while(1){var _qJ=B((function(_qK){var _qL=E(_qK);if(!_qL._){return __Z;}else{var _qM=_qL.b,_qN=E(_qL.a);if(!E(_qN.b)._){return new T2(1,_qN.a,new T(function(){return B(_qH(_qM));}));}else{_qI=_qM;return __continue;}}})(_qI));if(_qJ!=__continue){return _qJ;}}},_qO=new T(function(){return B(unCStr("!!: negative index"));}),_qP=new T(function(){return B(_O(_bT,_qO));}),_qQ=new T(function(){return B(err(_qP));}),_qR=new T(function(){return B(unCStr("!!: index too large"));}),_qS=new T(function(){return B(_O(_bT,_qR));}),_qT=new T(function(){return B(err(_qS));}),_qU=function(_qV,_qW){while(1){var _qX=E(_qV);if(!_qX._){return E(_qT);}else{var _qY=E(_qW);if(!_qY){return E(_qX.a);}else{_qV=_qX.b;_qW=_qY-1|0;continue;}}}},_qZ=function(_r0,_r1){if(_r1>=0){return new F(function(){return _qU(_r0,_r1);});}else{return E(_qQ);}},_r2=new T(function(){return B(unCStr("ACK"));}),_r3=new T(function(){return B(unCStr("BEL"));}),_r4=new T(function(){return B(unCStr("BS"));}),_r5=new T(function(){return B(unCStr("SP"));}),_r6=new T2(1,_r5,_u),_r7=new T(function(){return B(unCStr("US"));}),_r8=new T2(1,_r7,_r6),_r9=new T(function(){return B(unCStr("RS"));}),_ra=new T2(1,_r9,_r8),_rb=new T(function(){return B(unCStr("GS"));}),_rc=new T2(1,_rb,_ra),_rd=new T(function(){return B(unCStr("FS"));}),_re=new T2(1,_rd,_rc),_rf=new T(function(){return B(unCStr("ESC"));}),_rg=new T2(1,_rf,_re),_rh=new T(function(){return B(unCStr("SUB"));}),_ri=new T2(1,_rh,_rg),_rj=new T(function(){return B(unCStr("EM"));}),_rk=new T2(1,_rj,_ri),_rl=new T(function(){return B(unCStr("CAN"));}),_rm=new T2(1,_rl,_rk),_rn=new T(function(){return B(unCStr("ETB"));}),_ro=new T2(1,_rn,_rm),_rp=new T(function(){return B(unCStr("SYN"));}),_rq=new T2(1,_rp,_ro),_rr=new T(function(){return B(unCStr("NAK"));}),_rs=new T2(1,_rr,_rq),_rt=new T(function(){return B(unCStr("DC4"));}),_ru=new T2(1,_rt,_rs),_rv=new T(function(){return B(unCStr("DC3"));}),_rw=new T2(1,_rv,_ru),_rx=new T(function(){return B(unCStr("DC2"));}),_ry=new T2(1,_rx,_rw),_rz=new T(function(){return B(unCStr("DC1"));}),_rA=new T2(1,_rz,_ry),_rB=new T(function(){return B(unCStr("DLE"));}),_rC=new T2(1,_rB,_rA),_rD=new T(function(){return B(unCStr("SI"));}),_rE=new T2(1,_rD,_rC),_rF=new T(function(){return B(unCStr("SO"));}),_rG=new T2(1,_rF,_rE),_rH=new T(function(){return B(unCStr("CR"));}),_rI=new T2(1,_rH,_rG),_rJ=new T(function(){return B(unCStr("FF"));}),_rK=new T2(1,_rJ,_rI),_rL=new T(function(){return B(unCStr("VT"));}),_rM=new T2(1,_rL,_rK),_rN=new T(function(){return B(unCStr("LF"));}),_rO=new T2(1,_rN,_rM),_rP=new T(function(){return B(unCStr("HT"));}),_rQ=new T2(1,_rP,_rO),_rR=new T2(1,_r4,_rQ),_rS=new T2(1,_r3,_rR),_rT=new T2(1,_r2,_rS),_rU=new T(function(){return B(unCStr("ENQ"));}),_rV=new T2(1,_rU,_rT),_rW=new T(function(){return B(unCStr("EOT"));}),_rX=new T2(1,_rW,_rV),_rY=new T(function(){return B(unCStr("ETX"));}),_rZ=new T2(1,_rY,_rX),_s0=new T(function(){return B(unCStr("STX"));}),_s1=new T2(1,_s0,_rZ),_s2=new T(function(){return B(unCStr("SOH"));}),_s3=new T2(1,_s2,_s1),_s4=new T(function(){return B(unCStr("NUL"));}),_s5=new T2(1,_s4,_s3),_s6=92,_s7=new T(function(){return B(unCStr("\\DEL"));}),_s8=new T(function(){return B(unCStr("\\a"));}),_s9=new T(function(){return B(unCStr("\\\\"));}),_sa=new T(function(){return B(unCStr("\\SO"));}),_sb=new T(function(){return B(unCStr("\\r"));}),_sc=new T(function(){return B(unCStr("\\f"));}),_sd=new T(function(){return B(unCStr("\\v"));}),_se=new T(function(){return B(unCStr("\\n"));}),_sf=new T(function(){return B(unCStr("\\t"));}),_sg=new T(function(){return B(unCStr("\\b"));}),_sh=function(_si,_sj){if(_si<=127){var _sk=E(_si);switch(_sk){case 92:return new F(function(){return _O(_s9,_sj);});break;case 127:return new F(function(){return _O(_s7,_sj);});break;default:if(_sk<32){var _sl=E(_sk);switch(_sl){case 7:return new F(function(){return _O(_s8,_sj);});break;case 8:return new F(function(){return _O(_sg,_sj);});break;case 9:return new F(function(){return _O(_sf,_sj);});break;case 10:return new F(function(){return _O(_se,_sj);});break;case 11:return new F(function(){return _O(_sd,_sj);});break;case 12:return new F(function(){return _O(_sc,_sj);});break;case 13:return new F(function(){return _O(_sb,_sj);});break;case 14:return new F(function(){return _O(_sa,new T(function(){var _sm=E(_sj);if(!_sm._){return __Z;}else{if(E(_sm.a)==72){return B(unAppCStr("\\&",_sm));}else{return E(_sm);}}},1));});break;default:return new F(function(){return _O(new T2(1,_s6,new T(function(){return B(_qZ(_s5,_sl));})),_sj);});}}else{return new T2(1,_sk,_sj);}}}else{var _sn=new T(function(){var _so=jsShowI(_si);return B(_O(fromJSStr(_so),new T(function(){var _sp=E(_sj);if(!_sp._){return __Z;}else{var _sq=E(_sp.a);if(_sq<48){return E(_sp);}else{if(_sq>57){return E(_sp);}else{return B(unAppCStr("\\&",_sp));}}}},1)));});return new T2(1,_s6,_sn);}},_sr=new T(function(){return B(unCStr("\\\""));}),_ss=function(_st,_su){var _sv=E(_st);if(!_sv._){return E(_su);}else{var _sw=_sv.b,_sx=E(_sv.a);if(_sx==34){return new F(function(){return _O(_sr,new T(function(){return B(_ss(_sw,_su));},1));});}else{return new F(function(){return _sh(_sx,new T(function(){return B(_ss(_sw,_su));}));});}}},_sy=function(_sz,_sA,_sB){var _sC=function(_sD){var _sE=new T(function(){var _sF=new T(function(){var _sG=new T(function(){var _sH=new T(function(){var _sI=new T(function(){return B(unAppCStr("\', got unexpected ",new T2(1,_dA,new T(function(){return B(_ss(_sB,_dB));}))));},1);return B(_O(_sA,_sI));});return B(unAppCStr(") after \'",_sH));},1);return B(_O(_sz,_sG));});return B(unAppCStr("Expecting number (",_sF));});return new T1(0,_sE);},_sJ=B(_qH(B(_dD(_qG,_sB))));if(!_sJ._){return new F(function(){return _sC(_);});}else{if(!E(_sJ.b)._){return new T1(1,_sJ.a);}else{return new F(function(){return _sC(_);});}}},_sK=function(_sL,_sM){while(1){var _sN=B((function(_sO,_sP){var _sQ=E(_sP);if(!_sQ._){return __Z;}else{var _sR=_sQ.a,_sS=_sQ.b;if(!B(A1(_sO,_sR))){var _sT=_sO;_sL=_sT;_sM=_sS;return __continue;}else{return new T2(1,_sR,new T(function(){return B(_sK(_sO,_sS));}));}}})(_sL,_sM));if(_sN!=__continue){return _sN;}}},_sU=function(_sV,_sW,_sX){return new F(function(){return A1(_sV,new T(function(){return B(A1(_sW,_sX));}));});},_sY=function(_sZ){return (!E(_sZ))?true:false;},_t0=function(_t1,_t2){return new F(function(){return _sU(_sY,_t1,_t2);});},_t3=new T1(1,_t0),_t4=new T1(1,_2q),_t5=new T(function(){return B(unCStr("take"));}),_t6=new T(function(){return B(unCStr("strip"));}),_t7=new T(function(){return B(unCStr("only"));}),_t8=new T(function(){return B(unCStr("keep"));}),_t9=new T(function(){return B(unCStr("filter"));}),_ta=new T(function(){return B(unCStr("drop"));}),_tb=function(_tc){return (!B(_b9(_tc,_ta)))?(!B(_b9(_tc,_t9)))?(!B(_b9(_tc,_t8)))?(!B(_b9(_tc,_t7)))?(!B(_b9(_tc,_t6)))?(!B(_b9(_tc,_t5)))?__Z:E(_t4):E(_t3):E(_t4):E(_t4):E(_t4):E(_t3);},_td=function(_te){var _tf=E(_te);if(!_tf._){return __Z;}else{var _tg=_tf.b,_th=E(_tf.a);if(!_th._){var _ti=E(_th.a);if(_ti._==1){var _tj=B(_td(_tg));if(!_tj._){return new T2(1,_th,_u);}else{var _tk=E(_tj.a);if(!_tk._){var _tl=E(_tk.a);return (_tl._==1)?new T2(1,new T1(0,new T1(1,new T(function(){return B(_O(_ti.a,_tl.a));}))),_tj.b):new T2(1,_th,_tj);}else{return new T2(1,_th,_tj);}}}else{return new T2(1,_th,new T(function(){return B(_td(_tg));}));}}else{return new T2(1,_th,new T(function(){return B(_td(_tg));}));}}},_tm=function(_tn,_to){while(1){var _tp=E(_tn);if(!_tp._){return E(_to);}else{var _tq=new T2(1,_tp.a,_to);_tn=_tp.b;_to=_tq;continue;}}},_tr=function(_ts,_tt){var _tu=function(_tv){var _tw=E(_tv);return (_tw._==0)?0:(imul(B(_tu(_tw.b)),E(_ts))|0)+E(_tw.a)|0;};return new F(function(){return _tu(B(_tm(_tt,_u)));});},_tx=new T(function(){return B(unCStr(" "));}),_ty=new T1(2,_tx),_tz=new T(function(){return B(unCStr(","));}),_tA=new T1(2,_tz),_tB=new T1(2,_u),_tC=function(_tD){while(1){var _tE=E(_tD);if(!_tE._){return true;}else{if(E(_tE.a)==32){_tD=_tE.b;continue;}else{return false;}}}},_tF=function(_tG,_tH){if(E(_tG)==32){return new F(function(){return _tC(_tH);});}else{return false;}},_tI=function(_tJ){var _tK=E(_tJ);if(!_tK._){return E(_tB);}else{var _tL=_tK.b;switch(E(_tK.a)){case 32:var _tM=E(_tL);return (_tM._==0)?E(_ty):(!B(_tF(_tM.a,_tM.b)))?new T1(1,_tK):new T1(1,_tM);case 44:return (E(_tL)._==0)?E(_tA):new T1(1,_tK);default:return new T1(1,_tK);}}},_tN=new T(function(){return B(unCStr("\'\\\'\'"));}),_tO=39,_tP=function(_tQ,_tR){var _tS=E(_tQ);if(_tS==39){return new F(function(){return _O(_tN,_tR);});}else{return new T2(1,_tO,new T(function(){return B(_sh(_tS,new T2(1,_tO,_tR)));}));}},_tT=function(_tU){return new F(function(){return err(B(unAppCStr("genDigitToInt failed, unexpected char ",new T(function(){return B(_tP(_tU,_u));}))));});},_tV=function(_tW){var _tX=function(_tY){if(97>_tW){if(65>_tW){return new F(function(){return _tT(_tW);});}else{if(_tW>90){return new F(function(){return _tT(_tW);});}else{return (_tW-64|0)+9|0;}}}else{if(_tW>122){if(65>_tW){return new F(function(){return _tT(_tW);});}else{if(_tW>90){return new F(function(){return _tT(_tW);});}else{return (_tW-64|0)+9|0;}}}else{return (_tW-96|0)+9|0;}}};if(48>_tW){return new F(function(){return _tX(_);});}else{if(_tW>57){return new F(function(){return _tX(_);});}else{return _tW-48|0;}}},_tZ=function(_u0){return new F(function(){return _tV(E(_u0));});},_u1=function(_u2,_u3){return (_u2>10)?(48>_u3)?(97>_u3)?(65>_u3)?false:(_u3>90)?false:(_u3-64|0)<(_u2-9|0):(_u3>122)?(65>_u3)?false:(_u3>90)?false:(_u3-64|0)<(_u2-9|0):(_u3-96|0)<(_u2-9|0):(_u3>57)?(97>_u3)?(65>_u3)?false:(_u3>90)?false:(_u3-64|0)<(_u2-9|0):(_u3>122)?(65>_u3)?false:(_u3>90)?false:(_u3-64|0)<(_u2-9|0):(_u3-96|0)<(_u2-9|0):true:(48>_u3)?false:(_u3-48|0)<_u2;},_u4=function(_u5,_u6){return new F(function(){return _u1(E(_u5),E(_u6));});},_u7=function(_u8,_u9){var _ua=E(_u9);if(!_ua._){return __Z;}else{var _ub=_ua.a,_uc=new T(function(){var _ud=B(_5d(new T(function(){return B(A1(_u8,_ub));}),_ua.b));return new T2(0,_ud.a,_ud.b);});return new T2(1,new T2(1,_ub,new T(function(){return E(E(_uc).a);})),new T(function(){return B(_u7(_u8,E(_uc).b));}));}},_ue=new T(function(){return B(unCStr("head"));}),_uf=new T(function(){return B(_bU(_ue));}),_ug=function(_uh){var _ui=E(_uh);return (_ui._==0)?E(_uf):E(_ui.a);},_uj=function(_uk,_ul){var _um=function(_un){return (!B(A1(_uk,new T(function(){return B(_ug(_un));}))))?new T1(0,_un):new T1(1,_un);};return new F(function(){return _du(_um,B(_u7(function(_uo,_up){if(!B(A1(_uk,_uo))){return (!B(A1(_uk,_up)))?true:false;}else{return new F(function(){return A1(_uk,_up);});}},_ul)));});},_uq=function(_ur,_us){var _ut=function(_uu){var _uv=E(_uu);return (_uv._==0)?new T1(0,new T(function(){return B(_tI(_uv.a));})):new T1(1,new T(function(){return B(_tr(_ur,B(_du(_tZ,_uv.a))));}));};return new F(function(){return _du(_ut,B(_uj(function(_uw){return new F(function(){return _u4(_ur,_uw);});},_us)));});},_ux=function(_uy){while(1){var _uz=E(_uy);if(!_uz._){return true;}else{if(E(_uz.a)==32){_uy=_uz.b;continue;}else{return false;}}}},_uA=function(_uB){var _uC=E(_uB);if(!_uC._){return __Z;}else{if(E(_uC.a)==32){var _uD=E(_uC.b);return (_uD._==0)?E(_uC):(E(_uD.a)==32)?(!B(_ux(_uD.b)))?E(_uC):E(_uD):E(_uC);}else{return E(_uC);}}},_uE=function(_uF){var _uG=E(_uF);if(!_uG._){var _uH=E(_uG.a);return (_uH._==1)?new T1(0,new T1(1,new T(function(){return B(_uA(_uH.a));}))):E(_uG);}else{return E(_uG);}},_uI=function(_uJ,_uK){var _uL=function(_uM){var _uN=E(_uM);if(!_uN._){return __Z;}else{var _uO=_uN.b,_uP=E(_uN.a);if(!_uP._){return new T2(1,new T1(0,_uP.a),new T(function(){return B(_uL(_uO));}));}else{return new F(function(){return _O(B(_uq(_uJ,_uP.a)),new T(function(){return B(_uL(_uO));},1));});}}};return new F(function(){return _uL(B(_du(_dj,B(_dm(B(_du(_uE,B(_td(_uK)))))))));});},_uQ=function(_uR,_uS){var _uT=E(_uS);if(!_uT._){return new T2(0,_u,_u);}else{var _uU=_uT.a,_uV=_uT.b,_uW=E(_uR);if(_uW==1){return new T2(0,new T2(1,_uU,_u),_uV);}else{var _uX=new T(function(){var _uY=B(_uQ(_uW-1|0,_uV));return new T2(0,_uY.a,_uY.b);});return new T2(0,new T2(1,_uU,new T(function(){return E(E(_uX).a);})),new T(function(){return E(E(_uX).b);}));}}},_uZ=function(_v0,_v1){if(_v0>0){var _v2=B(_uQ(_v0,_v1)),_v3=E(_v2.a);if(!_v3._){return __Z;}else{var _v4=new T(function(){var _v5=function(_v6){var _v7=B(_uQ(_v0,_v6)),_v8=E(_v7.a);return (_v8._==0)?__Z:new T2(1,_v8,new T(function(){return B(_v5(_v7.b));}));};return B(_v5(_v2.b));});return new T2(1,_v3,_v4);}}else{return __Z;}},_v9=function(_va,_vb,_vc){var _vd=function(_ve){return (B(_gU(_ve,0))!=_vb)?new T1(0,new T1(0,_ve)):new T1(1,new T(function(){return B(_tr(_va,B(_du(_tZ,_ve))));}));};return new F(function(){return _du(_vd,B(_uZ(_vb,_vc)));});},_vf=function(_vg){while(1){var _vh=E(_vg);if(!_vh._){return true;}else{if(E(_vh.a)==32){_vg=_vh.b;continue;}else{return false;}}}},_vi=function(_vj,_vk){if(E(_vj)==32){return new F(function(){return _vf(_vk);});}else{return false;}},_vl=function(_vm){var _vn=E(_vm);if(!_vn._){return true;}else{var _vo=_vn.b,_vp=E(_vn.a);if(_vp==44){var _vq=E(_vo);if(!_vq._){return true;}else{return new F(function(){return _vi(44,_vq);});}}else{return new F(function(){return _vi(_vp,_vo);});}}},_vr=function(_vs){return (!B(_vl(_vs)))?new T1(1,_vs):new T1(2,_vs);},_vt=function(_vu){var _vv=E(_vu);return (_vv._==0)?new T1(0,new T(function(){return B(_vr(_vv.a));})):new T1(1,_vv.a);},_vw=function(_vx,_vy,_vz){var _vA=function(_vB){var _vC=E(_vB);if(!_vC._){return __Z;}else{var _vD=_vC.b,_vE=E(_vC.a);if(!_vE._){return new T2(1,new T1(0,_vE.a),new T(function(){return B(_vA(_vD));}));}else{var _vF=E(_vy),_vG=new T(function(){var _vH=function(_vI){var _vJ=E(_vI);if(!_vJ._){return __Z;}else{var _vK=_vJ.b,_vL=E(_vJ.a);if(!_vL._){return new T2(1,new T1(0,_vL.a),new T(function(){return B(_vH(_vK));}));}else{return new F(function(){return _O(B(_v9(_vx,_vF,_vL.a)),new T(function(){return B(_vH(_vK));},1));});}}};return B(_vH(_vD));},1);return new F(function(){return _O(B(_v9(_vx,_vF,_vE.a)),_vG);});}}};return new F(function(){return _vA(B(_du(_vt,B(_uj(function(_uw){return new F(function(){return _u4(_vx,_uw);});},_vz)))));});},_vM=function(_vN){return (97>_vN)?(65>_vN)?new T1(0,new T1(1,new T2(1,_vN,_u))):(_vN>90)?new T1(0,new T1(1,new T2(1,_vN,_u))):new T1(1,_vN-64|0):(_vN>122)?(65>_vN)?new T1(0,new T1(1,new T2(1,_vN,_u))):(_vN>90)?new T1(0,new T1(1,new T2(1,_vN,_u))):new T1(1,_vN-64|0):new T1(1,_vN-96|0);},_vO=function(_vP){return new F(function(){return _vM(E(_vP));});},_vQ=function(_vR){return new F(function(){return _du(_vO,_vR);});},_vS=function(_vT){return new F(function(){return _td(B(_d6(_vQ,_vT)));});},_vU=new T1(0,_vS),_vV=function(_vW){return 27-E(_vW)|0;},_vX=function(_vY){return new F(function(){return _aE(_vV,E(_vY));});},_vZ=function(_w0){var _w1=E(_w0);return (_w1._==0)?E(_w1):new T1(1,new T(function(){return B(_du(_vX,_w1.a));}));},_w2=function(_w3){return new F(function(){return _du(_vZ,_w3);});},_w4=new T1(1,_w2),_w5=new T(function(){return B(unCStr("+/-_="));}),_w6=function(_w7){while(1){var _w8=E(_w7);if(!_w8._){return false;}else{if(!E(_w8.a)){_w7=_w8.b;continue;}else{return true;}}}},_w9=function(_wa,_wb){if(!E(_wa)){return new F(function(){return _w6(_wb);});}else{return true;}},_wc=function(_wd){return new F(function(){return _w9(new T(function(){var _we=E(_wd);if(65>_we){return false;}else{return _we<=90;}}),new T2(1,new T(function(){var _wf=E(_wd);if(97>_wf){return false;}else{return _wf<=122;}}),new T2(1,new T(function(){var _wg=E(_wd);if(48>_wg){return false;}else{return _wg<=57;}}),new T2(1,new T(function(){return B(_bF(_bC,_wd,_w5));}),_u))));});},_wh=function(_wi){var _wj=E(_wi);return (_wj._==0)?new T1(0,new T1(0,_wj.a)):new T1(1,_wj.a);},_wk=63,_wl=new T1(1,_wk),_wm=62,_wn=new T1(1,_wm),_wo=new T(function(){return B(_O(_tN,_u));}),_wp=new T2(1,_tO,_u),_wq=function(_wr){var _ws=function(_wt){var _wu=function(_wv){var _ww=function(_wx){var _wy=E(_wr);switch(_wy){case 43:return E(_wn);case 45:return E(_wn);case 47:return E(_wl);case 95:return E(_wl);default:var _wz=new T(function(){var _wA=new T(function(){var _wB=E(_wy);if(_wB==39){return E(_wo);}else{return new T2(1,_tO,new T(function(){return B(_sh(_wB,_wp));}));}});return B(unAppCStr("Invalid base64 char: ",_wA));});return new T1(0,_wz);}};if(48>_wr){return new F(function(){return _ww(_);});}else{if(_wr>57){return new F(function(){return _ww(_);});}else{return new T1(1,(_wr-48|0)+52|0);}}};if(97>_wr){return new F(function(){return _wu(_);});}else{if(_wr>122){return new F(function(){return _wu(_);});}else{return new T1(1,(_wr-97|0)+26|0);}}};if(65>_wr){return new F(function(){return _ws(_);});}else{if(_wr>90){return new F(function(){return _ws(_);});}else{return new T1(1,_wr-65|0);}}},_wC=function(_wD,_wE){var _wF=E(_wE);if(_wF==1){return new T2(1,new T(function(){return E(_wD)&255;}),_u);}else{var _wG=new T(function(){return B(_wC(new T(function(){return E(_wD)>>8;}),_wF-1|0));});return new T2(1,new T(function(){return E(_wD)&255;}),_wG);}},_wH=function(_wI,_wJ){while(1){var _wK=E(_wI);if(!_wK._){return 0;}else{var _wL=_wK.b,_wM=E(_wJ);if(!_wM._){return 0;}else{var _wN=_wM.b,_wO=E(_wK.a);if(_wO<32){return E(_wM.a)<<_wO|B(_wH(_wL,_wN));}else{_wI=_wL;_wJ=_wN;continue;}}}}},_wP=function(_wQ,_wR){while(1){var _wS=E(_wQ);if(!_wS._){return 0;}else{var _wT=_wS.b,_wU=E(_wR);if(!_wU._){return 0;}else{var _wV=_wU.b,_wW=E(_wS.a);if(_wW<32){return E(_wU.a)<<_wW|B(_wP(_wT,_wV));}else{_wQ=_wT;_wR=_wV;continue;}}}}},_wX=function(_wY,_wZ){while(1){var _x0=E(_wY);if(!_x0._){return 0;}else{var _x1=_x0.b,_x2=E(_wZ);if(!_x2._){return 0;}else{var _x3=_x2.b,_x4=E(_x0.a);if(_x4<32){return E(_x2.a)<<_x4|B(_wX(_x1,_x3));}else{_wY=_x1;_wZ=_x3;continue;}}}}},_x5=function(_x6,_x7,_x8){if(_x8<=_x7){var _x9=new T(function(){var _xa=_x7-_x6|0,_xb=function(_xc){return (_xc>=(_x8-_xa|0))?new T2(1,_xc,new T(function(){return B(_xb(_xc+_xa|0));})):new T2(1,_xc,_u);};return B(_xb(_x7));});return new T2(1,_x6,_x9);}else{return (_x8<=_x6)?new T2(1,_x6,_u):__Z;}},_xd=function(_xe,_xf,_xg){if(_xg>=_xf){var _xh=new T(function(){var _xi=_xf-_xe|0,_xj=function(_xk){return (_xk<=(_xg-_xi|0))?new T2(1,_xk,new T(function(){return B(_xj(_xk+_xi|0));})):new T2(1,_xk,_u);};return B(_xj(_xf));});return new T2(1,_xe,_xh);}else{return (_xg>=_xe)?new T2(1,_xe,_u):__Z;}},_xl=function(_xm,_xn){if(_xn<_xm){return new F(function(){return _x5(_xm,_xn,-2147483648);});}else{return new F(function(){return _xd(_xm,_xn,2147483647);});}},_xo=new T(function(){return B(_xl(0,6));}),_xp=new T1(1,_u),_xq=new T(function(){return B(unCStr("base-64 has wrong number of characters"));}),_xr=new T1(0,_xq),_xs=function(_xt){var _xu=E(_xt);if(!_xu._){return E(_xp);}else{var _xv=_xu.a,_xw=E(_xu.b);if(!_xw._){return E(_xr);}else{var _xx=_xw.a,_xy=E(_xw.b);if(!_xy._){return E(_xr);}else{var _xz=E(_xy.b);if(!_xz._){return E(_xr);}else{var _xA=_xz.a,_xB=_xz.b,_xC=E(_xy.a),_xD=function(_xE){var _xF=E(_xA);if(_xF==61){var _xG=B(_wq(_xC));if(!_xG._){return new T1(0,_xG.a);}else{var _xH=B(_wq(E(_xx)));if(!_xH._){return new T1(0,_xH.a);}else{var _xI=B(_wq(E(_xv)));if(!_xI._){return new T1(0,_xI.a);}else{var _xJ=B(_xs(_xB));if(!_xJ._){return E(_xJ);}else{var _xK=new T(function(){return B(_O(B(_tm(B(_wC(new T(function(){return B(_wP(_xo,new T2(1,_xG.a,new T2(1,_xH.a,new T2(1,_xI.a,_u)))))>>2;}),2)),_u)),_xJ.a));});return new T1(1,_xK);}}}}}else{var _xL=B(_wq(_xF));if(!_xL._){return new T1(0,_xL.a);}else{var _xM=B(_wq(_xC));if(!_xM._){return new T1(0,_xM.a);}else{var _xN=B(_wq(E(_xx)));if(!_xN._){return new T1(0,_xN.a);}else{var _xO=B(_wq(E(_xv)));if(!_xO._){return new T1(0,_xO.a);}else{var _xP=B(_xs(_xB));if(!_xP._){return E(_xP);}else{var _xQ=new T(function(){return B(_O(B(_tm(B(_wC(new T(function(){return B(_wX(_xo,new T2(1,_xL.a,new T2(1,_xM.a,new T2(1,_xN.a,new T2(1,_xO.a,_u))))));}),3)),_u)),_xP.a));});return new T1(1,_xQ);}}}}}}};if(E(_xC)==61){if(E(_xA)==61){var _xR=B(_wq(E(_xx)));if(!_xR._){return new T1(0,_xR.a);}else{var _xS=B(_wq(E(_xv)));if(!_xS._){return new T1(0,_xS.a);}else{var _xT=B(_xs(_xB));if(!_xT._){return E(_xT);}else{var _xU=new T(function(){return B(_O(B(_tm(B(_wC(new T(function(){return B(_wH(_xo,new T2(1,_xR.a,new T2(1,_xS.a,_u))))>>4;}),1)),_u)),_xT.a));});return new T1(1,_xU);}}}}else{return new F(function(){return _xD(_);});}}else{return new F(function(){return _xD(_);});}}}}}},_xV=function(_xW){var _xX=E(_xW);if(!_xX._){return __Z;}else{var _xY=_xX.b,_xZ=E(_xX.a);if(!_xZ._){return new T2(1,new T1(0,_xZ.a),new T(function(){return B(_xV(_xY));}));}else{var _y0=B(_xs(_xZ.a));if(!_y0._){return new T2(1,new T1(0,new T1(0,_y0.a)),new T(function(){return B(_xV(_xY));}));}else{var _y1=new T(function(){return B(_xV(_xY));}),_y2=function(_y3){var _y4=E(_y3);return (_y4._==0)?E(_y1):new T2(1,new T1(1,_y4.a),new T(function(){return B(_y2(_y4.b));}));};return new F(function(){return _y2(_y0.a);});}}}},_y5=function(_y6){var _y7=E(_y6);if(!_y7._){return __Z;}else{var _y8=_y7.b,_y9=E(_y7.a);if(!_y9._){return new T2(1,new T1(0,_y9.a),new T(function(){return B(_y5(_y8));}));}else{return new F(function(){return _O(B(_xV(B(_du(_wh,B(_uj(_wc,_y9.a)))))),new T(function(){return B(_y5(_y8));},1));});}}},_ya=function(_yb){return new F(function(){return _y5(B(_dy(_yb)));});},_yc=new T1(0,_ya),_yd=function(_ye,_yf){while(1){var _yg=E(_ye);if(!_yg._){return (E(_yf)._==0)?1:0;}else{var _yh=E(_yf);if(!_yh._){return 2;}else{var _yi=E(_yg.a),_yj=E(_yh.a);if(_yi!=_yj){return (_yi>_yj)?2:0;}else{_ye=_yg.b;_yf=_yh.b;continue;}}}}},_yk=function(_yl,_ym){while(1){var _yn=E(_yl),_yo=E(_ym);if(!_yo._){switch(B(_yd(_yn,_yo.b))){case 0:_yl=_yn;_ym=_yo.d;continue;case 1:return new T1(1,_yo.c);default:_yl=_yn;_ym=_yo.e;continue;}}else{return __Z;}}},_yp=new T1(0,_ty),_yq=new T1(1,_yp),_yr=new T(function(){return B(unCStr(" / "));}),_ys=function(_yt){var _yu=E(_yt);if(!_yu._){var _yv=E(_yu.a);switch(_yv._){case 1:var _yw=E(_yv.a);return (_yw._==0)?__Z:(E(_yw.a)==32)?(E(_yw.b)._==0)?__Z:(!B(_b9(_yw,_yr)))?new T1(1,_yu):E(_yq):(!B(_b9(_yw,_yr)))?new T1(1,_yu):E(_yq);case 2:return __Z;default:return new T1(1,_yu);}}else{return new T1(1,_yu);}},_yx=function(_yy,_yz,_yA,_yB){var _yC=E(_yy);if(_yC==1){var _yD=E(_yB);return (_yD._==0)?new T3(0,new T(function(){return new T5(0,1,E(_yz),_yA,E(_6t),E(_6t));}),_u,_u):(B(_yd(_yz,E(_yD.a).a))==0)?new T3(0,new T(function(){return new T5(0,1,E(_yz),_yA,E(_6t),E(_6t));}),_yD,_u):new T3(0,new T(function(){return new T5(0,1,E(_yz),_yA,E(_6t),E(_6t));}),_u,_yD);}else{var _yE=B(_yx(_yC>>1,_yz,_yA,_yB)),_yF=_yE.a,_yG=_yE.c,_yH=E(_yE.b);if(!_yH._){return new T3(0,_yF,_u,_yG);}else{var _yI=E(_yH.a),_yJ=_yI.a,_yK=_yI.b,_yL=E(_yH.b);if(!_yL._){return new T3(0,new T(function(){return B(_7g(_yJ,_yK,_yF));}),_u,_yG);}else{var _yM=E(_yL.a),_yN=_yM.a;if(!B(_yd(_yJ,_yN))){var _yO=B(_yx(_yC>>1,_yN,_yM.b,_yL.b));return new T3(0,new T(function(){return B(_8J(_yJ,_yK,_yF,_yO.a));}),_yO.b,_yO.c);}else{return new T3(0,_yF,_u,_yH);}}}}},_yP=function(_yQ,_yR,_yS){var _yT=E(_yQ),_yU=E(_yS);if(!_yU._){var _yV=_yU.b,_yW=_yU.c,_yX=_yU.d,_yY=_yU.e;switch(B(_yd(_yT,_yV))){case 0:return new F(function(){return _7p(_yV,_yW,B(_yP(_yT,_yR,_yX)),_yY);});break;case 1:return new T5(0,_yU.a,E(_yT),_yR,E(_yX),E(_yY));default:return new F(function(){return _6y(_yV,_yW,_yX,B(_yP(_yT,_yR,_yY)));});}}else{return new T5(0,1,E(_yT),_yR,E(_6t),E(_6t));}},_yZ=function(_z0,_z1){while(1){var _z2=E(_z1);if(!_z2._){return E(_z0);}else{var _z3=E(_z2.a),_z4=B(_yP(_z3.a,_z3.b,_z0));_z0=_z4;_z1=_z2.b;continue;}}},_z5=function(_z6,_z7,_z8,_z9){return new F(function(){return _yZ(B(_yP(_z7,_z8,_z6)),_z9);});},_za=function(_zb,_zc,_zd){var _ze=E(_zc);return new F(function(){return _yZ(B(_yP(_ze.a,_ze.b,_zb)),_zd);});},_zf=function(_zg,_zh,_zi){while(1){var _zj=E(_zi);if(!_zj._){return E(_zh);}else{var _zk=E(_zj.a),_zl=_zk.a,_zm=_zk.b,_zn=E(_zj.b);if(!_zn._){return new F(function(){return _7g(_zl,_zm,_zh);});}else{var _zo=E(_zn.a),_zp=_zo.a;if(!B(_yd(_zl,_zp))){var _zq=B(_yx(_zg,_zp,_zo.b,_zn.b)),_zr=_zq.a,_zs=E(_zq.c);if(!_zs._){var _zt=_zg<<1,_zu=B(_8J(_zl,_zm,_zh,_zr));_zg=_zt;_zh=_zu;_zi=_zq.b;continue;}else{return new F(function(){return _za(B(_8J(_zl,_zm,_zh,_zr)),_zs.a,_zs.b);});}}else{return new F(function(){return _z5(_zh,_zl,_zm,_zn);});}}}}},_zv=function(_zw,_zx,_zy,_zz,_zA){var _zB=E(_zA);if(!_zB._){return new F(function(){return _7g(_zy,_zz,_zx);});}else{var _zC=E(_zB.a),_zD=_zC.a;if(!B(_yd(_zy,_zD))){var _zE=B(_yx(_zw,_zD,_zC.b,_zB.b)),_zF=_zE.a,_zG=E(_zE.c);if(!_zG._){return new F(function(){return _zf(_zw<<1,B(_8J(_zy,_zz,_zx,_zF)),_zE.b);});}else{return new F(function(){return _za(B(_8J(_zy,_zz,_zx,_zF)),_zG.a,_zG.b);});}}else{return new F(function(){return _z5(_zx,_zy,_zz,_zB);});}}},_zH=function(_zI){var _zJ=E(_zI);if(!_zJ._){return new T0(1);}else{var _zK=E(_zJ.a),_zL=_zK.a,_zM=_zK.b,_zN=E(_zJ.b);if(!_zN._){return new T5(0,1,E(_zL),_zM,E(_6t),E(_6t));}else{var _zO=_zN.b,_zP=E(_zN.a),_zQ=_zP.a,_zR=_zP.b;if(!B(_yd(_zL,_zQ))){return new F(function(){return _zv(1,new T5(0,1,E(_zL),_zM,E(_6t),E(_6t)),_zQ,_zR,_zO);});}else{return new F(function(){return _z5(new T5(0,1,E(_zL),_zM,E(_6t),E(_6t)),_zQ,_zR,_zO);});}}}},_zS=new T(function(){return B(unCStr("-..."));}),_zT=98,_zU=new T2(0,_zT,_zS),_zV=new T(function(){return B(unCStr("-.-."));}),_zW=99,_zX=new T2(0,_zW,_zV),_zY=new T(function(){return B(unCStr("-.."));}),_zZ=100,_A0=new T2(0,_zZ,_zY),_A1=new T(function(){return B(unCStr("."));}),_A2=101,_A3=new T2(0,_A2,_A1),_A4=new T(function(){return B(unCStr("..-."));}),_A5=102,_A6=new T2(0,_A5,_A4),_A7=new T(function(){return B(unCStr("--."));}),_A8=103,_A9=new T2(0,_A8,_A7),_Aa=new T(function(){return B(unCStr("...."));}),_Ab=104,_Ac=new T2(0,_Ab,_Aa),_Ad=new T(function(){return B(unCStr(".."));}),_Ae=105,_Af=new T2(0,_Ae,_Ad),_Ag=new T(function(){return B(unCStr("---"));}),_Ah=111,_Ai=new T2(0,_Ah,_Ag),_Aj=new T(function(){return B(unCStr(".--."));}),_Ak=112,_Al=new T2(0,_Ak,_Aj),_Am=new T(function(){return B(unCStr("--.-"));}),_An=113,_Ao=new T2(0,_An,_Am),_Ap=new T(function(){return B(unCStr(".-."));}),_Aq=114,_Ar=new T2(0,_Aq,_Ap),_As=new T(function(){return B(unCStr("..."));}),_At=115,_Au=new T2(0,_At,_As),_Av=new T(function(){return B(unCStr("-"));}),_Aw=116,_Ax=new T2(0,_Aw,_Av),_Ay=new T(function(){return B(unCStr("..-"));}),_Az=117,_AA=new T2(0,_Az,_Ay),_AB=new T(function(){return B(unCStr("...-"));}),_AC=118,_AD=new T2(0,_AC,_AB),_AE=new T(function(){return B(unCStr(".--"));}),_AF=119,_AG=new T2(0,_AF,_AE),_AH=new T(function(){return B(unCStr("-..-"));}),_AI=120,_AJ=new T2(0,_AI,_AH),_AK=new T(function(){return B(unCStr("-.--"));}),_AL=121,_AM=new T2(0,_AL,_AK),_AN=new T(function(){return B(unCStr("--.."));}),_AO=122,_AP=new T2(0,_AO,_AN),_AQ=new T(function(){return B(unCStr("-----"));}),_AR=48,_AS=new T2(0,_AR,_AQ),_AT=new T(function(){return B(unCStr(".----"));}),_AU=49,_AV=new T2(0,_AU,_AT),_AW=new T(function(){return B(unCStr("..---"));}),_AX=50,_AY=new T2(0,_AX,_AW),_AZ=new T(function(){return B(unCStr("..--.-"));}),_B0=95,_B1=new T2(0,_B0,_AZ),_B2=new T2(1,_B1,_u),_B3=new T(function(){return B(unCStr("-.--.-"));}),_B4=41,_B5=new T2(0,_B4,_B3),_B6=new T2(1,_B5,_B2),_B7=new T(function(){return B(unCStr("-.--."));}),_B8=40,_B9=new T2(0,_B8,_B7),_Ba=new T2(1,_B9,_B6),_Bb=new T(function(){return B(unCStr("-..-."));}),_Bc=47,_Bd=new T2(0,_Bc,_Bb),_Be=new T2(1,_Bd,_Ba),_Bf=new T(function(){return B(unCStr("-....-"));}),_Bg=45,_Bh=new T2(0,_Bg,_Bf),_Bi=new T2(1,_Bh,_Be),_Bj=new T(function(){return B(unCStr(".----."));}),_Bk=39,_Bl=new T2(0,_Bk,_Bj),_Bm=new T2(1,_Bl,_Bi),_Bn=new T(function(){return B(unCStr("---..."));}),_Bo=58,_Bp=new T2(0,_Bo,_Bn),_Bq=new T2(1,_Bp,_Bm),_Br=new T(function(){return B(unCStr("-.-.-."));}),_Bs=59,_Bt=new T2(0,_Bs,_Br),_Bu=new T2(1,_Bt,_Bq),_Bv=new T(function(){return B(unCStr("..--.."));}),_Bw=63,_Bx=new T2(0,_Bw,_Bv),_By=new T2(1,_Bx,_Bu),_Bz=new T(function(){return B(unCStr(".-.-.-"));}),_BA=46,_BB=new T2(0,_BA,_Bz),_BC=new T2(1,_BB,_By),_BD=new T(function(){return B(unCStr("--..--"));}),_BE=44,_BF=new T2(0,_BE,_BD),_BG=new T2(1,_BF,_BC),_BH=new T(function(){return B(unCStr("----."));}),_BI=57,_BJ=new T2(0,_BI,_BH),_BK=new T2(1,_BJ,_BG),_BL=new T(function(){return B(unCStr("---.."));}),_BM=56,_BN=new T2(0,_BM,_BL),_BO=new T2(1,_BN,_BK),_BP=new T(function(){return B(unCStr("--..."));}),_BQ=55,_BR=new T2(0,_BQ,_BP),_BS=new T2(1,_BR,_BO),_BT=new T(function(){return B(unCStr("-...."));}),_BU=54,_BV=new T2(0,_BU,_BT),_BW=new T2(1,_BV,_BS),_BX=new T(function(){return B(unCStr("....."));}),_BY=53,_BZ=new T2(0,_BY,_BX),_C0=new T2(1,_BZ,_BW),_C1=new T(function(){return B(unCStr("....-"));}),_C2=52,_C3=new T2(0,_C2,_C1),_C4=new T2(1,_C3,_C0),_C5=51,_C6=new T(function(){return B(unCStr("...--"));}),_C7=new T2(0,_C5,_C6),_C8=new T2(1,_C7,_C4),_C9=new T2(1,_AY,_C8),_Ca=new T2(1,_AV,_C9),_Cb=new T2(1,_AS,_Ca),_Cc=new T2(1,_AP,_Cb),_Cd=new T2(1,_AM,_Cc),_Ce=new T2(1,_AJ,_Cd),_Cf=new T2(1,_AG,_Ce),_Cg=new T2(1,_AD,_Cf),_Ch=new T2(1,_AA,_Cg),_Ci=new T2(1,_Ax,_Ch),_Cj=new T2(1,_Au,_Ci),_Ck=new T2(1,_Ar,_Cj),_Cl=new T2(1,_Ao,_Ck),_Cm=new T2(1,_Al,_Cl),_Cn=new T2(1,_Ai,_Cm),_Co=new T(function(){return B(unCStr("-."));}),_Cp=110,_Cq=new T2(0,_Cp,_Co),_Cr=new T2(1,_Cq,_Cn),_Cs=new T(function(){return B(unCStr("--"));}),_Ct=109,_Cu=new T2(0,_Ct,_Cs),_Cv=new T2(1,_Cu,_Cr),_Cw=new T(function(){return B(unCStr(".-.."));}),_Cx=108,_Cy=new T2(0,_Cx,_Cw),_Cz=new T2(1,_Cy,_Cv),_CA=new T(function(){return B(unCStr("-.-"));}),_CB=107,_CC=new T2(0,_CB,_CA),_CD=new T2(1,_CC,_Cz),_CE=new T(function(){return B(unCStr(".---"));}),_CF=106,_CG=new T2(0,_CF,_CE),_CH=new T2(1,_CG,_CD),_CI=new T2(1,_Af,_CH),_CJ=new T2(1,_Ac,_CI),_CK=new T2(1,_A9,_CJ),_CL=new T2(1,_A6,_CK),_CM=new T2(1,_A3,_CL),_CN=new T2(1,_A0,_CM),_CO=new T2(1,_zX,_CN),_CP=new T2(1,_zU,_CO),_CQ=new T(function(){return B(unCStr(".-"));}),_CR=97,_CS=new T2(0,_CR,_CQ),_CT=new T2(1,_CS,_CP),_CU=function(_CV){var _CW=E(_CV);return new T2(0,_CW.b,_CW.a);},_CX=new T(function(){return B(_du(_CU,_CT));}),_CY=new T(function(){return B(_zH(_CX));}),_CZ=function(_D0){while(1){var _D1=B((function(_D2){var _D3=E(_D2);if(!_D3._){return __Z;}else{var _D4=_D3.b,_D5=B(_ys(_D3.a));if(!_D5._){_D0=_D4;return __continue;}else{return new T2(1,new T(function(){var _D6=E(_D5.a);if(!_D6._){return E(_D6);}else{var _D7=_D6.a,_D8=B(_yk(_D7,_CY));if(!_D8._){return new T1(0,new T1(0,_D7));}else{return new T1(1,new T2(1,_D8.a,_u));}}}),new T(function(){return B(_CZ(_D4));}));}}})(_D0));if(_D1!=__continue){return _D1;}}},_D9=function(_Da){return new F(function(){return _bF(_bC,_Da,_CQ);});},_Db=function(_Dc){var _Dd=E(_Dc);if(!_Dd._){return __Z;}else{var _De=_Dd.b,_Df=E(_Dd.a);if(!_Df._){return new T2(1,_Df,new T(function(){return B(_Db(_De));}));}else{var _Dg=new T(function(){return B(_Db(_De));}),_Dh=function(_Di){var _Dj=E(_Di);return (_Dj._==0)?E(_Dg):new T2(1,new T(function(){var _Dk=E(_Dj.a);if(!_Dk._){return new T1(0,new T1(1,_Dk.a));}else{return new T1(1,_Dk.a);}}),new T(function(){return B(_Dh(_Dj.b));}));};return new F(function(){return _Dh(B(_uj(_D9,_Df.a)));});}}},_Dl=function(_Dm){return new F(function(){return _CZ(B(_Db(B(_du(_dj,B(_dm(_Dm)))))));});},_Dn=new T1(1,_Dl),_Do=new T(function(){return B(unCStr("pshift"));}),_Dp=new T(function(){return B(unCStr("shift amount"));}),_Dq=function(_Dr){return E(_Dr)+13|0;},_Ds=function(_Dt){return new F(function(){return _aE(_Dq,E(_Dt));});},_Du=function(_Dv){var _Dw=E(_Dv);return (_Dw._==0)?E(_Dw):new T1(1,new T(function(){return B(_du(_Ds,_Dw.a));}));},_Dx=function(_Dy){return new F(function(){return _du(_Du,_Dy);});},_Dz=new T1(1,_Dx),_DA=function(_DB){var _DC=E(_DB);if(33>_DC){return E(_DC);}else{if(_DC>126){return E(_DC);}else{var _DD=33+B(_ax((_DC-33|0)+47|0,94))|0;if(_DD>>>0>1114111){return new F(function(){return _av(_DD);});}else{return _DD;}}}},_DE=function(_DF){var _DG=E(_DF);return (_DG._==0)?E(_DG):new T1(1,new T(function(){return B(_du(_DA,_DG.a));}));},_DH=function(_DI){return new F(function(){return _du(_DE,_DI);});},_DJ=new T1(1,_DH),_DK=new T(function(){return B(unCStr("shift"));}),_DL=function(_DM,_DN){var _DO=E(_DN);if(!_DO._){return __Z;}else{var _DP=_DO.b,_DQ=E(_DO.a);if(!_DQ._){return new T2(1,_DQ,new T(function(){return B(_DL(_DM,_DP));}));}else{var _DR=B(_DL(_DM,_DP));return (_DR._==0)?new T2(1,_DQ,_u):(E(_DR.a)._==0)?new T2(1,_DQ,_DR):new T2(1,_DQ,new T2(1,_DM,_DR));}}},_DS=function(_DT,_DU){while(1){var _DV=E(_DU);if(!_DV._){var _DW=E(_DV.b);if(_DT!=_DW){if(_DT>_DW){_DU=_DV.e;continue;}else{_DU=_DV.d;continue;}}else{return new T1(1,_DV.c);}}else{return __Z;}}},_DX=new T(function(){return B(unCStr(" / "));}),_DY=new T1(1,_DX),_DZ=new T1(0,_DY),_E0=new T(function(){return B(_ab(_CT));}),_E1=function(_E2){var _E3=E(_E2);if(!_E3._){return __Z;}else{var _E4=_E3.b,_E5=E(_E3.a);if(!_E5._){return new T2(1,new T1(0,new T(function(){var _E6=E(_E5.a);if(_E6._==2){var _E7=E(_E6.a);if(!_E7._){return E(_E6);}else{if(E(_E7.a)==32){if(!E(_E7.b)._){return E(_DY);}else{return E(_E6);}}else{return E(_E6);}}}else{return E(_E6);}})),new T(function(){return B(_E1(_E4));}));}else{var _E8=new T(function(){return B(_E1(_E4));}),_E9=function(_Ea){var _Eb=E(_Ea);return (_Eb._==0)?E(_E8):new T2(1,new T(function(){var _Ec=E(_Eb.a),_Ed=E(_Ec);if(_Ed==32){return E(_DZ);}else{var _Ee=u_towlower(_Ed);if(_Ee>>>0>1114111){return B(_av(_Ee));}else{var _Ef=B(_DS(_Ee,_E0));if(!_Ef._){return new T1(0,new T1(1,new T2(1,_Ec,_u)));}else{return new T1(1,_Ef.a);}}}}),new T(function(){return B(_E9(_Eb.b));}));};return new F(function(){return _E9(_E5.a);});}}},_Eg=function(_Eh){return new F(function(){return _DL(_yp,B(_E1(_Eh)));});},_Ei=new T1(1,_Eg),_Ej=new T(function(){return B(unCStr("morse"));}),_Ek=new T(function(){return B(unCStr("to"));}),_El=new T(function(){return B(unCStr("rot47"));}),_Em=new T(function(){return B(unCStr("rot13"));}),_En=new T(function(){return B(unCStr("base64"));}),_Eo=new T(function(){return B(unCStr("atbash"));}),_Ep=new T(function(){return B(unCStr("alpha"));}),_Eq=10,_Er=function(_Es){var _Et=E(_Es);return (E(_Et)==32)?E(_Eq):E(_Et);},_Eu=function(_Ev){return new F(function(){return _du(_Er,_Ev);});},_Ew=function(_Ex,_Ey){var _Ez=function(_EA){var _EB=E(_EA);if(!_EB._){var _EC=E(_EB.a);switch(_EC._){case 0:return new T1(0,new T1(0,new T(function(){return B(A1(_Ex,_EC.a));})));case 1:return new T1(0,new T1(1,new T(function(){return B(A1(_Ex,_EC.a));})));case 2:return new T1(0,new T1(2,new T(function(){return B(A1(_Ex,_EC.a));})));default:return E(_EB);}}else{return new T1(1,new T(function(){return B(A1(_Ex,_EB.a));}));}};return new F(function(){return _du(_Ez,_Ey);});},_ED=function(_Ev){return new F(function(){return _Ew(_Eu,_Ev);});},_EE=new T1(1,_ED),_EF=new T(function(){return B(unCStr("translate with empty from string"));}),_EG=new T(function(){return B(err(_EF));}),_EH=new T(function(){return B(unCStr("translate"));}),_EI=new T(function(){return B(unCStr("split-lines"));}),_EJ=function(_EK){return new T1(1,_EK);},_EL=function(_EM){while(1){var _EN=B((function(_EO){var _EP=E(_EO);if(!_EP._){return __Z;}else{var _EQ=_EP.b,_ER=E(_EP.a);if(!_ER._){_EM=_EQ;return __continue;}else{return new T2(1,_ER.a,new T(function(){return B(_EL(_EQ));}));}}})(_EM));if(_EN!=__continue){return _EN;}}},_ES=function(_ET){return new F(function(){return _du(_EJ,B(_EL(_ET)));});},_EU=new T1(1,_ES),_EV=new T(function(){return B(unCStr("CxBadInt "));}),_EW=new T(function(){return B(unCStr("CxDelim "));}),_EX=new T(function(){return B(unCStr("CxExtra "));}),_EY=new T(function(){return B(unCStr("CxBadString "));}),_EZ=function(_F0,_F1,_F2){var _F3=E(_F1);switch(_F3._){case 0:var _F4=_F3.a;if(_F0<11){return new F(function(){return _O(_EY,new T2(1,_dA,new T(function(){return B(_ss(_F4,new T2(1,_dA,_F2)));})));});}else{var _F5=new T(function(){return B(_O(_EY,new T2(1,_dA,new T(function(){return B(_ss(_F4,new T2(1,_dA,new T2(1,_3c,_F2))));}))));});return new T2(1,_3d,_F5);}break;case 1:var _F6=_F3.a;if(_F0<11){return new F(function(){return _O(_EX,new T2(1,_dA,new T(function(){return B(_ss(_F6,new T2(1,_dA,_F2)));})));});}else{var _F7=new T(function(){return B(_O(_EX,new T2(1,_dA,new T(function(){return B(_ss(_F6,new T2(1,_dA,new T2(1,_3c,_F2))));}))));});return new T2(1,_3d,_F7);}break;case 2:var _F8=_F3.a;if(_F0<11){return new F(function(){return _O(_EW,new T2(1,_dA,new T(function(){return B(_ss(_F8,new T2(1,_dA,_F2)));})));});}else{var _F9=new T(function(){return B(_O(_EW,new T2(1,_dA,new T(function(){return B(_ss(_F8,new T2(1,_dA,new T2(1,_3c,_F2))));}))));});return new T2(1,_3d,_F9);}break;default:var _Fa=_F3.a;if(_F0<11){return new F(function(){return _O(_EV,new T(function(){return B(_3e(11,E(_Fa),_F2));},1));});}else{var _Fb=new T(function(){return B(_O(_EV,new T(function(){return B(_3e(11,E(_Fa),new T2(1,_3c,_F2)));},1)));});return new T2(1,_3d,_Fb);}}},_Fc=function(_Fd){return new F(function(){return _EZ(0,_Fd,_u);});},_Fe=function(_Ff,_Fg){return new F(function(){return _EZ(0,_Ff,_Fg);});},_Fh=function(_Fi,_Fj){return new F(function(){return _1S(_Fe,_Fi,_Fj);});},_Fk=function(_Fl,_Fm,_Fn){return new F(function(){return _EZ(E(_Fl),_Fm,_Fn);});},_Fo=new T3(0,_Fk,_Fc,_Fh),_Fp=new T2(1,_dA,_u),_Fq=function(_Fr){return new T2(1,_dA,new T(function(){return B(_ss(_Fr,_Fp));}));},_Fs=function(_Ft,_Fu){return new T2(1,_dA,new T(function(){return B(_ss(_Ft,new T2(1,_dA,_Fu)));}));},_Fv=function(_Fw,_Fx,_Fy){return new F(function(){return _Fs(_Fx,_Fy);});},_Fz=function(_FA,_FB){return new F(function(){return _1S(_Fs,_FA,_FB);});},_FC=new T3(0,_Fv,_Fq,_Fz),_FD=11,_FE=new T(function(){return B(unCStr("Right "));}),_FF=new T(function(){return B(unCStr("Left "));}),_FG=function(_FH){return E(E(_FH).a);},_FI=function(_FJ,_FK,_FL,_FM){var _FN=E(_FM);if(!_FN._){var _FO=new T(function(){return B(A3(_FG,_FJ,_FD,_FN.a));});if(_FL<11){var _FP=function(_FQ){return new F(function(){return _O(_FF,new T(function(){return B(A1(_FO,_FQ));},1));});};return E(_FP);}else{var _FR=function(_FS){var _FT=new T(function(){return B(_O(_FF,new T(function(){return B(A1(_FO,new T2(1,_3c,_FS)));},1)));});return new T2(1,_3d,_FT);};return E(_FR);}}else{var _FU=new T(function(){return B(A3(_FG,_FK,_FD,_FN.a));});if(_FL<11){var _FV=function(_FW){return new F(function(){return _O(_FE,new T(function(){return B(A1(_FU,_FW));},1));});};return E(_FV);}else{var _FX=function(_FY){var _FZ=new T(function(){return B(_O(_FE,new T(function(){return B(A1(_FU,new T2(1,_3c,_FY)));},1)));});return new T2(1,_3d,_FZ);};return E(_FX);}}},_G0=function(_G1){return new F(function(){return _FI(_Fo,_FC,0,_G1);});},_G2=function(_G3){return new T2(1,new T1(1,new T(function(){return B(_1S(_G0,_G3,_u));})),_u);},_G4=new T1(1,_G2),_G5=new T(function(){return B(unCStr("raw"));}),_G6=new T(function(){return B(unCStr("purify"));}),_G7=new T(function(){return B(unCStr("char"));}),_G8=new T(function(){return B(unCStr("Could not parse string coder: "));}),_G9=new T1(1,_2q),_Ga=new T1(1,_G9),_Gb=new T(function(){return B(unCStr("base"));}),_Gc=new T(function(){return B(unCStr("radix"));}),_Gd=function(_Ge,_Gf){var _Gg=E(_Gf);return (_Gg._==0)?new T1(0,_Gg.a):new T1(1,new T(function(){return B(A1(_Ge,_Gg.a));}));},_Gh=32,_Gi=function(_Gj){while(1){var _Gk=E(_Gj);if(!_Gk._){return true;}else{if(E(_Gk.a)==32){_Gj=_Gk.b;continue;}else{return false;}}}},_Gl=function(_Gm){var _Gn=E(_Gm);return (_Gn._==0)?__Z:(E(_Gn.a)==32)?(!B(_Gi(_Gn.b)))?E(_Gn):new T2(1,_Gh,_Gn):E(_Gn);},_Go=function(_Gp){var _Gq=E(_Gp);if(!_Gq._){var _Gr=E(_Gq.a);return (_Gr._==1)?new T1(0,new T1(1,new T(function(){return B(_Gl(_Gr.a));}))):E(_Gq);}else{return E(_Gq);}},_Gs=2,_Gt=16,_Gu=new T1(1,_u),_Gv=new T(function(){return B(unCStr("base"));}),_Gw=new T(function(){return B(unCStr("GHC.Exception"));}),_Gx=new T(function(){return B(unCStr("ArithException"));}),_Gy=new T5(0,new Long(4194982440,719304104,true),new Long(3110813675,1843557400,true),_Gv,_Gw,_Gx),_Gz=new T5(0,new Long(4194982440,719304104,true),new Long(3110813675,1843557400,true),_Gy,_u,_u),_GA=function(_GB){return E(_Gz);},_GC=function(_GD){var _GE=E(_GD);return new F(function(){return _A(B(_y(_GE.a)),_GA,_GE.b);});},_GF=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_GG=new T(function(){return B(unCStr("denormal"));}),_GH=new T(function(){return B(unCStr("divide by zero"));}),_GI=new T(function(){return B(unCStr("loss of precision"));}),_GJ=new T(function(){return B(unCStr("arithmetic underflow"));}),_GK=new T(function(){return B(unCStr("arithmetic overflow"));}),_GL=function(_GM,_GN){switch(E(_GM)){case 0:return new F(function(){return _O(_GK,_GN);});break;case 1:return new F(function(){return _O(_GJ,_GN);});break;case 2:return new F(function(){return _O(_GI,_GN);});break;case 3:return new F(function(){return _O(_GH,_GN);});break;case 4:return new F(function(){return _O(_GG,_GN);});break;default:return new F(function(){return _O(_GF,_GN);});}},_GO=function(_GP){return new F(function(){return _GL(_GP,_u);});},_GQ=function(_GR,_GS,_GT){return new F(function(){return _GL(_GS,_GT);});},_GU=function(_GV,_GW){return new F(function(){return _1S(_GL,_GV,_GW);});},_GX=new T3(0,_GQ,_GO,_GU),_GY=new T(function(){return new T5(0,_GA,_GX,_GZ,_GC,_GO);}),_GZ=function(_5c){return new T2(0,_GY,_5c);},_H0=3,_H1=new T(function(){return B(_GZ(_H0));}),_H2=new T(function(){return die(_H1);}),_H3=0,_H4=new T(function(){return B(_GZ(_H3));}),_H5=new T(function(){return die(_H4);}),_H6=function(_H7,_H8,_H9){var _Ha=E(_H8);if(!_Ha){var _Hb=E(_H9);return (_Hb==0)?new T1(1,new T(function(){return B(_tm(_u,_u));})):new T1(0,new T1(3,_Hb));}else{if(_H9>=0){var _Hc=E(_H7);switch(_Hc){case -1:var _Hd=E(_H9);if(_Hd==(-2147483648)){return E(_H5);}else{var _He=quotRemI(_Hd,-1),_Hf=function(_Hg,_Hh){var _Hi=E(_Hg);if(!_Hi){return (E(_Hh)==0)?E(_Gu):__Z;}else{if(_Hh>=0){var _Hj=E(_Hh);if(_Hj==(-2147483648)){return E(_H5);}else{var _Hk=quotRemI(_Hj,-1),_Hl=B(_Hf(_Hi-1|0,_Hk.a));return (_Hl._==0)?__Z:new T1(1,new T2(1,_Hk.b,_Hl.a));}}else{return __Z;}}},_Hm=B(_Hf(_Ha-1|0,_He.a));return (_Hm._==0)?new T1(0,new T1(3,_Hd)):new T1(1,new T(function(){return B(_tm(new T2(1,_He.b,_Hm.a),_u));}));}break;case 0:return E(_H2);default:var _Hn=quotRemI(_H9,_Hc),_Ho=function(_Hp,_Hq){var _Hr=E(_Hp);if(!_Hr){return (E(_Hq)==0)?E(_Gu):__Z;}else{if(_Hq>=0){var _Hs=quotRemI(_Hq,_Hc),_Ht=B(_Ho(_Hr-1|0,_Hs.a));return (_Ht._==0)?__Z:new T1(1,new T2(1,_Hs.b,_Ht.a));}else{return __Z;}}},_Hu=B(_Ho(_Ha-1|0,_Hn.a));return (_Hu._==0)?new T1(0,new T1(3,_H9)):new T1(1,new T(function(){return B(_tm(new T2(1,_Hn.b,_Hu.a),_u));}));}}else{return new T1(0,new T1(3,_H9));}}},_Hv=function(_Hw){if(_Hw>9){var _Hx=(97+_Hw|0)-10|0;if(_Hx>>>0>1114111){return new F(function(){return _av(_Hx);});}else{return _Hx;}}else{var _Hy=48+_Hw|0;if(_Hy>>>0>1114111){return new F(function(){return _av(_Hy);});}else{return _Hy;}}},_Hz=function(_HA){return new F(function(){return _Hv(E(_HA));});},_HB=function(_HC,_HD,_HE){var _HF=B(_H6(_HC,_HD,_HE));return (_HF._==0)?new T1(0,_HF.a):new T1(1,new T(function(){return B(_du(_Hz,_HF.a));}));},_HG=function(_HH,_HI,_HJ){return new F(function(){return _HB(_HH,E(_HI),E(_HJ));});},_HK=function(_HL,_HM,_HN){return new F(function(){return _DL(_yp,B(_du(function(_HO){var _HP=E(_HO);if(!_HP._){return new T1(0,_HP.a);}else{return new F(function(){return _HG(_HL,_HM,_HP.a);});}},_HN)));});},_HQ=function(_HR){var _HS=E(_HR);return (_HS._==0)?E(_HS):new T1(1,new T(function(){return B(_du(_bk,_HS.a));}));},_HT=function(_HU){return new F(function(){return _du(_Go,B(_td(B(_du(_HQ,B(_HK(_Gt,_Gs,_HU)))))));});},_HV=new T(function(){return B(unCStr("no such delimiter"));}),_HW=function(_HX){return new F(function(){return err(_HV);});},_HY=new T(function(){return B(_HW(_));}),_HZ=function(_I0,_I1){var _I2=function(_I3){while(1){var _I4=B((function(_I5){var _I6=E(_I5);if(!_I6._){return __Z;}else{var _I7=_I6.b,_I8=E(_I6.a);if(!_I8._){var _I9=E(_I8.a);if(_I9._==2){var _Ia=E(_I9.a);if(!_Ia._){_I3=_I7;return __continue;}else{var _Ib=_Ia.b;switch(E(_Ia.a)){case 32:var _Ic=E(_Ib);if(!_Ic._){_I3=_I7;return __continue;}else{return new T2(1,new T1(0,new T1(1,_Ic)),new T(function(){return B(_I2(_I7));}));}break;case 44:if(!E(_Ib)._){_I3=_I7;return __continue;}else{return E(_HY);}break;default:return E(_HY);}}}else{return new T2(1,new T1(0,_I9),new T(function(){return B(_I2(_I7));}));}}else{var _Id=new T(function(){var _Ie=E(_I8.a);if(0>_Ie){return new T1(0,new T1(3,_Ie));}else{if(_Ie>=E(_I0)){return new T1(0,new T1(3,_Ie));}else{return new T1(1,new T2(1,new T(function(){return B(_Hv(_Ie));}),_u));}}});return new T2(1,_Id,new T(function(){return B(_I2(_I7));}));}}})(_I3));if(_I4!=__continue){return _I4;}}};return new F(function(){return _I2(_I1);});},_If=function(_Ig){return new F(function(){return _du(_HQ,B(_HZ(_Gt,_Ig)));});},_Ih=function(_Ev){return new F(function(){return _HZ(_Gs,_Ev);});},_Ii=function(_Ij){return new F(function(){return _du(_Go,B(_td(B(_HK(_Gt,_Gs,_Ij)))));});},_Ik=10,_Il=0,_Im=new T2(1,_Il,_u),_In=0,_Io=new T(function(){return E(_H5);}),_Ip=new T2(1,_In,_Io),_Iq=function(_Ir,_Is){var _It=E(_Is);if(!_It){return E(_Im);}else{var _Iu=E(_Ir);switch(_Iu){case -1:var _Iv=E(_It);if(_Iv==(-2147483648)){return new F(function(){return _tm(_Ip,_u);});}else{var _Iw=quotRemI(_Iv,-1),_Ix=new T(function(){var _Iy=function(_Iz){var _IA=E(_Iz);switch(_IA){case -2147483648:return E(_Ip);case 0:return __Z;default:var _IB=quotRemI(_IA,-1);return new T2(1,_IB.b,new T(function(){return B(_Iy(_IB.a));}));}};return B(_Iy(_Iw.a));});return new F(function(){return _tm(new T2(1,_Iw.b,_Ix),_u);});}break;case 0:return E(_H2);default:var _IC=quotRemI(_It,_Iu),_ID=new T(function(){var _IE=function(_IF){var _IG=E(_IF);if(!_IG){return __Z;}else{var _IH=quotRemI(_IG,_Iu);return new T2(1,_IH.b,new T(function(){return B(_IE(_IH.a));}));}};return B(_IE(_IC.a));});return new F(function(){return _tm(new T2(1,_IC.b,_ID),_u);});}}},_II=function(_IJ,_IK){var _IL=function(_IM){var _IN=E(_IM);return (_IN._==0)?new T1(0,_IN.a):new T1(1,new T(function(){return B(_du(_Hz,B(_Iq(_IJ,E(_IN.a)))));}));};return new F(function(){return _du(_Go,B(_td(B(_DL(_yp,B(_du(_IL,_IK)))))));});},_IO=function(_Ev){return new F(function(){return _II(_Ik,_Ev);});},_IP=function(_Ev){return new F(function(){return _HZ(_Gt,_Ev);});},_IQ=function(_IR,_IS){if(_IR<=_IS){var _IT=function(_IU){return new T2(1,_IU,new T(function(){if(_IU!=_IS){return B(_IT(_IU+1|0));}else{return __Z;}}));};return new F(function(){return _IT(_IR);});}else{return __Z;}},_IV=function(_IW){return new F(function(){return _IQ(E(_IW),2147483647);});},_IX=function(_IY,_IZ){return new F(function(){return _xl(E(_IY),E(_IZ));});},_J0=function(_J1,_J2,_J3){if(_J2<_J1){return new F(function(){return _x5(_J1,_J2,_J3);});}else{return new F(function(){return _xd(_J1,_J2,_J3);});}},_J4=function(_J5,_J6,_J7){return new F(function(){return _J0(E(_J5),E(_J6),E(_J7));});},_J8=function(_J9,_Ja){return new F(function(){return _IQ(E(_J9),E(_Ja));});},_Jb=function(_Jc){return E(_Jc);},_Jd=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_Je=new T(function(){return B(err(_Jd));}),_Jf=function(_Jg){var _Jh=E(_Jg);return (_Jh==(-2147483648))?E(_Je):_Jh-1|0;},_Ji=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_Jj=new T(function(){return B(err(_Ji));}),_Jk=function(_Jl){var _Jm=E(_Jl);return (_Jm==2147483647)?E(_Jj):_Jm+1|0;},_Jn={_:0,a:_Jk,b:_Jf,c:_Jb,d:_Jb,e:_IV,f:_IX,g:_J8,h:_J4},_Jo=function(_Jp,_Jq){if(_Jp<=0){if(_Jp>=0){return new F(function(){return quot(_Jp,_Jq);});}else{if(_Jq<=0){return new F(function(){return quot(_Jp,_Jq);});}else{return quot(_Jp+1|0,_Jq)-1|0;}}}else{if(_Jq>=0){if(_Jp>=0){return new F(function(){return quot(_Jp,_Jq);});}else{if(_Jq<=0){return new F(function(){return quot(_Jp,_Jq);});}else{return quot(_Jp+1|0,_Jq)-1|0;}}}else{return quot(_Jp-1|0,_Jq)-1|0;}}},_Jr=function(_Js,_Jt){var _Ju=E(_Jt);switch(_Ju){case -1:var _Jv=E(_Js);if(_Jv==(-2147483648)){return E(_H5);}else{return new F(function(){return _Jo(_Jv,-1);});}break;case 0:return E(_H2);default:return new F(function(){return _Jo(_Js,_Ju);});}},_Jw=function(_Jx,_Jy){return new F(function(){return _Jr(E(_Jx),E(_Jy));});},_Jz=new T2(0,_H5,_In),_JA=function(_JB,_JC){var _JD=E(_JB),_JE=E(_JC);switch(_JE){case -1:var _JF=E(_JD);if(_JF==(-2147483648)){return E(_Jz);}else{if(_JF<=0){if(_JF>=0){var _JG=quotRemI(_JF,-1);return new T2(0,_JG.a,_JG.b);}else{var _JH=quotRemI(_JF,-1);return new T2(0,_JH.a,_JH.b);}}else{var _JI=quotRemI(_JF-1|0,-1);return new T2(0,_JI.a-1|0,(_JI.b+(-1)|0)+1|0);}}break;case 0:return E(_H2);default:if(_JD<=0){if(_JD>=0){var _JJ=quotRemI(_JD,_JE);return new T2(0,_JJ.a,_JJ.b);}else{if(_JE<=0){var _JK=quotRemI(_JD,_JE);return new T2(0,_JK.a,_JK.b);}else{var _JL=quotRemI(_JD+1|0,_JE);return new T2(0,_JL.a-1|0,(_JL.b+_JE|0)-1|0);}}}else{if(_JE>=0){if(_JD>=0){var _JM=quotRemI(_JD,_JE);return new T2(0,_JM.a,_JM.b);}else{if(_JE<=0){var _JN=quotRemI(_JD,_JE);return new T2(0,_JN.a,_JN.b);}else{var _JO=quotRemI(_JD+1|0,_JE);return new T2(0,_JO.a-1|0,(_JO.b+_JE|0)-1|0);}}}else{var _JP=quotRemI(_JD-1|0,_JE);return new T2(0,_JP.a-1|0,(_JP.b+_JE|0)+1|0);}}}},_JQ=function(_JR,_JS){var _JT=E(_JS);switch(_JT){case -1:return E(_In);case 0:return E(_H2);default:return new F(function(){return _ax(E(_JR),_JT);});}},_JU=function(_JV,_JW){var _JX=E(_JV),_JY=E(_JW);switch(_JY){case -1:var _JZ=E(_JX);if(_JZ==(-2147483648)){return E(_H5);}else{return new F(function(){return quot(_JZ,-1);});}break;case 0:return E(_H2);default:return new F(function(){return quot(_JX,_JY);});}},_K0=function(_K1,_K2){var _K3=E(_K1),_K4=E(_K2);switch(_K4){case -1:var _K5=E(_K3);if(_K5==(-2147483648)){return E(_Jz);}else{var _K6=quotRemI(_K5,-1);return new T2(0,_K6.a,_K6.b);}break;case 0:return E(_H2);default:var _K7=quotRemI(_K3,_K4);return new T2(0,_K7.a,_K7.b);}},_K8=function(_K9,_Ka){var _Kb=E(_Ka);switch(_Kb){case -1:return E(_In);case 0:return E(_H2);default:return E(_K9)%_Kb;}},_Kc=function(_Kd){return new F(function(){return _gZ(E(_Kd));});},_Ke=new T1(0,1),_Kf=function(_Kg){return new T2(0,E(B(_gZ(E(_Kg)))),E(_Ke));},_Kh=function(_Ki,_Kj){return E(_Ki)-E(_Kj)|0;},_Kk=function(_Kl){var _Km=E(_Kl);return (_Km<0)? -_Km:E(_Km);},_Kn=function(_Ko){return new F(function(){return _in(_Ko);});},_Kp=function(_Kq){return  -E(_Kq);},_Kr=-1,_Ks=0,_Kt=1,_Ku=function(_Kv){var _Kw=E(_Kv);return (_Kw>=0)?(E(_Kw)==0)?E(_Ks):E(_Kt):E(_Kr);},_Kx={_:0,a:_6q,b:_Kh,c:_6n,d:_Kp,e:_Kk,f:_Ku,g:_Kn},_Ky=function(_Kz,_KA){return E(_Kz)==E(_KA);},_KB=function(_KC,_KD){return E(_KC)!=E(_KD);},_KE=new T2(0,_Ky,_KB),_KF=function(_KG,_KH){var _KI=E(_KG),_KJ=E(_KH);return (_KI>_KJ)?E(_KI):E(_KJ);},_KK=function(_KL,_KM){var _KN=E(_KL),_KO=E(_KM);return (_KN>_KO)?E(_KO):E(_KN);},_KP=function(_KQ,_KR){return (_KQ>=_KR)?(_KQ!=_KR)?2:1:0;},_KS=function(_KT,_KU){return new F(function(){return _KP(E(_KT),E(_KU));});},_KV=function(_KW,_KX){return E(_KW)>=E(_KX);},_KY=function(_KZ,_L0){return E(_KZ)>E(_L0);},_L1=function(_L2,_L3){return E(_L2)<=E(_L3);},_L4=function(_L5,_L6){return E(_L5)<E(_L6);},_L7={_:0,a:_KE,b:_KS,c:_L4,d:_L1,e:_KY,f:_KV,g:_KF,h:_KK},_L8=new T3(0,_Kx,_L7,_Kf),_L9={_:0,a:_L8,b:_Jn,c:_JU,d:_K8,e:_Jw,f:_JQ,g:_K0,h:_JA,i:_Kc},_La=function(_Lb){return E(E(_Lb).a);},_Lc=function(_Ld){return E(E(_Ld).a);},_Le=function(_Lf){return E(E(_Lf).a);},_Lg=function(_Lh){return E(E(_Lh).b);},_Li=function(_Lj){return E(E(_Lj).g);},_Lk=function(_Ll){return E(E(_Ll).f);},_Lm=new T1(0,0),_Ln=function(_Lo,_Lp,_Lq){var _Lr=new T(function(){return B(A3(_Lk,_Lo,_Lp,_Lq));}),_Ls=B(_La(_Lo));return (!B(A3(_bD,B(_Lc(B(_Lg(_Ls)))),_Lr,new T(function(){return B(A2(_Li,B(_Le(_Ls)),_Lm));}))))?E(_Lr):E(_Lq);},_Lt=function(_Lu,_Lv){return new F(function(){return _Ln(_L9,_Lv,_Lu);});},_Lw=new T1(1,_Lt),_Lx=new T(function(){return B(unCStr("mod1"));}),_Ly=function(_Lz,_LA){return new F(function(){return _JQ(_LA,_Lz);});},_LB=new T1(1,_Ly),_LC=new T(function(){return B(unCStr("mod"));}),_LD=new T1(1,_6n),_LE=new T(function(){return B(unCStr("times"));}),_LF=function(_LG,_LH){return new F(function(){return _Kh(_LH,_LG);});},_LI=new T1(1,_LF),_LJ=new T(function(){return B(unCStr("subtract"));}),_LK=new T(function(){return B(unCStr("multiply"));}),_LL=new T(function(){return B(unCStr("minus"));}),_LM=new T1(1,_6q),_LN=new T(function(){return B(unCStr("plus"));}),_LO=new T(function(){return B(unCStr("add"));}),_LP=function(_LQ){if(!B(_b9(_LQ,_LO))){if(!B(_b9(_LQ,_LN))){var _LR=function(_LS){if(!B(_b9(_LQ,_LL))){var _LT=function(_LU){if(!B(_b9(_LQ,_LK))){if(!B(_b9(_LQ,_LJ))){if(!B(_b9(_LQ,_LE))){var _LV=function(_LW){return (!B(_b9(_LQ,_LC)))?(!B(_b9(_LQ,_Lx)))?__Z:E(_Lw):E(_LB);},_LX=E(_LQ);if(!_LX._){return new F(function(){return _LV(_);});}else{var _LY=_LX.b;switch(E(_LX.a)){case 42:if(!E(_LY)._){return E(_LD);}else{return new F(function(){return _LV(_);});}break;case 120:if(!E(_LY)._){return E(_LD);}else{return new F(function(){return _LV(_);});}break;default:return new F(function(){return _LV(_);});}}}else{return E(_LD);}}else{return E(_LI);}}else{return E(_LD);}},_LZ=E(_LQ);if(!_LZ._){return new F(function(){return _LT(_);});}else{if(E(_LZ.a)==45){if(!E(_LZ.b)._){return E(_LI);}else{return new F(function(){return _LT(_);});}}else{return new F(function(){return _LT(_);});}}}else{return E(_LI);}},_M0=E(_LQ);if(!_M0._){return new F(function(){return _LR(_);});}else{if(E(_M0.a)==43){if(!E(_M0.b)._){return E(_LM);}else{return new F(function(){return _LR(_);});}}else{return new F(function(){return _LR(_);});}}}else{return E(_LM);}}else{return E(_LM);}},_M1=function(_M2){return (1>_M2)?new T1(0,new T1(3,_M2)):(_M2>26)?new T1(0,new T1(3,_M2)):new T1(1,new T2(1,new T(function(){var _M3=96+_M2|0;if(_M3>>>0>1114111){return B(_av(_M3));}else{var _M4=u_towupper(_M3);if(_M4>>>0>1114111){return B(_av(_M4));}else{return _M4;}}}),_u));},_M5=function(_M6){return new F(function(){return _M1(E(_M6));});},_M7=function(_M8){while(1){var _M9=B((function(_Ma){var _Mb=E(_Ma);if(!_Mb._){return __Z;}else{var _Mc=_Mb.b,_Md=E(_Mb.a);if(!_Md._){var _Me=E(_Md.a);if(_Me._==2){var _Mf=E(_Me.a);if(!_Mf._){_M8=_Mc;return __continue;}else{var _Mg=_Mf.b;switch(E(_Mf.a)){case 32:var _Mh=E(_Mg);if(!_Mh._){_M8=_Mc;return __continue;}else{return new T2(1,new T1(0,new T1(1,_Mh)),new T(function(){return B(_M7(_Mc));}));}break;case 44:if(!E(_Mg)._){_M8=_Mc;return __continue;}else{return E(_HY);}break;default:return E(_HY);}}}else{return new T2(1,new T1(0,_Me),new T(function(){return B(_M7(_Mc));}));}}else{return new T2(1,new T(function(){return B(_M5(_Md.a));}),new T(function(){return B(_M7(_Mc));}));}}})(_M8));if(_M9!=__continue){return _M9;}}},_Mi=function(_Mj){return new F(function(){return _M7(_Mj);});},_Mk=new T1(1,_Mi),_Ml=function(_Mm){return (1>_Mm)?new T1(0,new T1(3,_Mm)):(_Mm>26)?new T1(0,new T1(3,_Mm)):new T1(1,new T2(1,new T(function(){var _Mn=96+_Mm|0;if(_Mn>>>0>1114111){return B(_av(_Mn));}else{return _Mn;}}),_u));},_Mo=function(_Mp){return new F(function(){return _Ml(E(_Mp));});},_Mq=function(_Mr){while(1){var _Ms=B((function(_Mt){var _Mu=E(_Mt);if(!_Mu._){return __Z;}else{var _Mv=_Mu.b,_Mw=E(_Mu.a);if(!_Mw._){var _Mx=E(_Mw.a);if(_Mx._==2){var _My=E(_Mx.a);if(!_My._){_Mr=_Mv;return __continue;}else{var _Mz=_My.b;switch(E(_My.a)){case 32:var _MA=E(_Mz);if(!_MA._){_Mr=_Mv;return __continue;}else{return new T2(1,new T1(0,new T1(1,_MA)),new T(function(){return B(_Mq(_Mv));}));}break;case 44:if(!E(_Mz)._){_Mr=_Mv;return __continue;}else{return E(_HY);}break;default:return E(_HY);}}}else{return new T2(1,new T1(0,_Mx),new T(function(){return B(_Mq(_Mv));}));}}else{return new T2(1,new T(function(){return B(_Mo(_Mw.a));}),new T(function(){return B(_Mq(_Mv));}));}}})(_Mr));if(_Ms!=__continue){return _Ms;}}},_MB=function(_MC){return new F(function(){return _Mq(_MC);});},_MD=new T1(1,_MB),_ME=function(_MF){var _MG=E(_MF);if(!_MG._){return E(_MG);}else{var _MH=E(_MG.a);return (0>_MH)?new T1(0,new T1(3,_MH)):(_MH>=256)?new T1(0,new T1(3,_MH)):E(_MG);}},_MI=function(_MJ){return new F(function(){return err(B(unAppCStr("Out of range for base 64: ",new T(function(){return B(_3e(0,_MJ,_u));}))));});},_MK=function(_ML){var _MM=function(_MN){var _MO=function(_MP){if(52>_ML){var _MQ=E(_ML);switch(_MQ){case 62:return 43;case 63:return 47;default:return new F(function(){return _MI(_MQ);});}}else{if(_ML>=62){var _MR=E(_ML);switch(_MR){case 62:return 43;case 63:return 47;default:return new F(function(){return _MI(_MR);});}}else{var _MS=(48+_ML|0)-52|0;if(_MS>>>0>1114111){return new F(function(){return _av(_MS);});}else{return _MS;}}}};if(26>_ML){return new F(function(){return _MO(_);});}else{if(_ML>=52){return new F(function(){return _MO(_);});}else{var _MT=(97+_ML|0)-26|0;if(_MT>>>0>1114111){return new F(function(){return _av(_MT);});}else{return _MT;}}}};if(0>_ML){return new F(function(){return _MM(_);});}else{if(_ML>=26){return new F(function(){return _MM(_);});}else{var _MU=65+_ML|0;if(_MU>>>0>1114111){return new F(function(){return _av(_MU);});}else{return _MU;}}}},_MV=function(_MW,_MX){var _MY=E(_MX);if(_MY==1){return new T2(1,new T(function(){return B(_MK(E(_MW)&63));}),_u);}else{var _MZ=new T(function(){return B(_MV(new T(function(){return E(_MW)>>6;}),_MY-1|0));});return new T2(1,new T(function(){return B(_MK(E(_MW)&63));}),_MZ);}},_N0=function(_N1,_N2){while(1){var _N3=E(_N1);if(!_N3._){return 0;}else{var _N4=_N3.b,_N5=E(_N2);if(!_N5._){return 0;}else{var _N6=_N5.b,_N7=E(_N3.a);if(_N7<32){return E(_N5.a)<<_N7|B(_N0(_N4,_N6));}else{_N1=_N4;_N2=_N6;continue;}}}}},_N8=function(_N9,_Na){while(1){var _Nb=E(_N9);if(!_Nb._){return 0;}else{var _Nc=_Nb.b,_Nd=E(_Na);if(!_Nd._){return 0;}else{var _Ne=_Nd.b,_Nf=E(_Nb.a);if(_Nf<32){return E(_Nd.a)<<_Nf|B(_N8(_Nc,_Ne));}else{_N9=_Nc;_Na=_Ne;continue;}}}}},_Ng=function(_Nh,_Ni){while(1){var _Nj=E(_Nh);if(!_Nj._){return 0;}else{var _Nk=_Nj.b,_Nl=E(_Ni);if(!_Nl._){return 0;}else{var _Nm=_Nl.b,_Nn=E(_Nj.a);if(_Nn<32){return E(_Nl.a)<<_Nn|B(_Ng(_Nk,_Nm));}else{_Nh=_Nk;_Ni=_Nm;continue;}}}}},_No=new T(function(){return B(unCStr("="));}),_Np=new T(function(){return B(unCStr("=="));}),_Nq=new T(function(){return B(_xl(0,8));}),_Nr=function(_Ns){var _Nt=E(_Ns);if(!_Nt._){return __Z;}else{var _Nu=_Nt.a,_Nv=E(_Nt.b);if(!_Nv._){return new F(function(){return _O(B(_tm(B(_MV(new T(function(){return B(_Ng(_Nq,new T2(1,_Nu,_u)))<<4;}),2)),_u)),_Np);});}else{var _Nw=_Nv.a,_Nx=E(_Nv.b);if(!_Nx._){return new F(function(){return _O(B(_tm(B(_MV(new T(function(){return B(_N8(_Nq,new T2(1,_Nw,new T2(1,_Nu,_u))))<<2;}),3)),_u)),_No);});}else{return new F(function(){return _O(B(_tm(B(_MV(new T(function(){return B(_N0(_Nq,new T2(1,_Nx.a,new T2(1,_Nw,new T2(1,_Nu,_u)))));}),4)),_u)),new T(function(){return B(_Nr(_Nx.b));},1));});}}}},_Ny=function(_Nz){var _NA=E(_Nz);return (_NA._==0)?new T1(0,_NA.a):new T1(1,new T(function(){return B(_Nr(_NA.a));}));},_NB=function(_NC){return new F(function(){return _du(_Ny,B(_dm(B(_du(_ME,_NC)))));});},_ND=new T1(1,_NB),_NE=function(_NF){var _NG=E(_NF);if(_NG>>>0>1114111){return new F(function(){return _av(_NG);});}else{return _NG;}},_NH=function(_NI){return new T2(1,new T(function(){return B(_NE(_NI));}),_u);},_NJ=function(_NK){while(1){var _NL=B((function(_NM){var _NN=E(_NM);if(!_NN._){return __Z;}else{var _NO=_NN.b,_NP=E(_NN.a);if(!_NP._){var _NQ=E(_NP.a);if(_NQ._==2){var _NR=E(_NQ.a);if(!_NR._){_NK=_NO;return __continue;}else{var _NS=_NR.b;switch(E(_NR.a)){case 32:var _NT=E(_NS);if(!_NT._){_NK=_NO;return __continue;}else{return new T2(1,new T1(0,new T1(1,_NT)),new T(function(){return B(_NJ(_NO));}));}break;case 44:if(!E(_NS)._){_NK=_NO;return __continue;}else{return E(_HY);}break;default:return E(_HY);}}}else{return new T2(1,new T1(0,_NQ),new T(function(){return B(_NJ(_NO));}));}}else{return new T2(1,new T1(1,new T(function(){return B(_NH(_NP.a));})),new T(function(){return B(_NJ(_NO));}));}}})(_NK));if(_NL!=__continue){return _NL;}}},_NU=function(_NV){return new F(function(){return _NJ(_NV);});},_NW=new T1(1,_NU),_NX=function(_NY){return (33>_NY)?new T1(0,new T1(3,_NY)):(_NY>126)?new T1(0,new T1(3,_NY)):new T1(1,new T2(1,new T(function(){if(_NY>>>0>1114111){return B(_av(_NY));}else{return _NY;}}),_u));},_NZ=function(_O0){return new F(function(){return _NX(E(_O0));});},_O1=function(_O2){while(1){var _O3=B((function(_O4){var _O5=E(_O4);if(!_O5._){return __Z;}else{var _O6=_O5.b,_O7=E(_O5.a);if(!_O7._){var _O8=E(_O7.a);if(_O8._==2){var _O9=E(_O8.a);if(!_O9._){_O2=_O6;return __continue;}else{var _Oa=_O9.b;switch(E(_O9.a)){case 32:var _Ob=E(_Oa);if(!_Ob._){_O2=_O6;return __continue;}else{return new T2(1,new T1(0,new T1(1,_Ob)),new T(function(){return B(_O1(_O6));}));}break;case 44:if(!E(_Oa)._){_O2=_O6;return __continue;}else{return E(_HY);}break;default:return E(_HY);}}}else{return new T2(1,new T1(0,_O8),new T(function(){return B(_O1(_O6));}));}}else{return new T2(1,new T(function(){return B(_NZ(_O7.a));}),new T(function(){return B(_O1(_O6));}));}}})(_O2));if(_O3!=__continue){return _O3;}}},_Oc=function(_Od){return new F(function(){return _O1(_Od);});},_Oe=new T1(1,_Oc),_Of=new T(function(){return B(unCStr("printable"));}),_Og=new T(function(){return B(unCStr("Alpha"));}),_Oh=new T(function(){return B(unCStr("nybble"));}),_Oi=new T(function(){return B(unCStr("number"));}),_Oj=new T(function(){return B(unCStr("byte"));}),_Ok=new T(function(){return B(unCStr("bit"));}),_Ol=new T(function(){return B(unCStr("Nybble"));}),_Om=new T(function(){return B(unCStr("Byte"));}),_On=new T(function(){return B(unCStr("operand"));}),_Oo=function(_Op){var _Oq=E(_Op);return (_Oq._==0)?E(_Oq):new T1(1,new T(function(){return B(_Kp(_Oq.a));}));},_Or=function(_Ev){return new F(function(){return _du(_Oo,_Ev);});},_Os=new T1(0,_Or),_Ot=function(_Ou){return new F(function(){return _du(_EJ,B(_EL(_Ou)));});},_Ov=new T1(0,_Ot),_Ow=function(_Ox,_Oy,_Oz){return new F(function(){return _3e(E(_Ox),E(_Oy),_Oz);});},_OA=function(_OB,_OC){return new F(function(){return _3e(0,E(_OB),_OC);});},_OD=function(_OE,_OF){return new F(function(){return _1S(_OA,_OE,_OF);});},_OG=new T3(0,_Ow,_5Y,_OD),_OH=function(_OI){return new F(function(){return _FI(_Fo,_OG,0,_OI);});},_OJ=function(_OK){return new T2(1,new T1(1,new T(function(){return B(_1S(_OH,_OK,_u));})),_u);},_OL=new T1(1,_OJ),_OM=new T(function(){return B(unCStr("negated"));}),_ON=new T(function(){return B(unCStr("negate"));}),_OO=new T(function(){return B(unCStr("Could not parse int coder: "));}),_OP=new T1(0,_2q),_OQ=new T1(1,_OP),_OR=new T(function(){return B(unCStr(" after \'to\'"));}),_OS=new T(function(){return B(unCStr("Unexpected end after \'to\'"));}),_OT=new T(function(){return B(_O(_OO,_OS));}),_OU=new T1(0,_OT),_OV=function(_OW){var _OX=E(_OW);return new F(function(){return _OY(_OX.a,_OX.b);});},_OZ=function(_P0){var _P1=E(_P0);if(!_P1._){return E(_bX);}else{return new F(function(){return _bO(_P1.a,_P1.b);});}},_P2=1,_P3=new T2(0,_aS,_P2),_P4=new T1(1,_P3),_P5=new T(function(){return B(unCStr("Expecting \'bit[s]\', \'byte[s]\', \'digit[s]\', or \'nybble[s]\'"));}),_P6=new T1(0,_P5),_P7=new T(function(){return B(unCStr("nybble"));}),_P8=new T(function(){return B(unCStr("byte"));}),_P9=new T(function(){return B(unCStr("bit"));}),_Pa=new T2(0,_b2,_aS),_Pb=new T1(1,_Pa),_Pc=new T2(0,_b4,_P2),_Pd=new T1(1,_Pc),_Pe=new T2(0,_b2,_P2),_Pf=new T1(1,_Pe),_Pg=function(_Ph,_Pi){while(1){var _Pj=E(_Ph);if(!_Pj._){return E(_Pi);}else{_Ph=_Pj.b;_Pi=_Pj.a;continue;}}},_Pk=function(_Pl){if(E(B(_Pg(_Pl,_bZ)))==115){var _Pm=B(_OZ(_Pl));}else{var _Pm=E(_Pl);}return (!B(_b9(_Pm,_P9)))?(!B(_b9(_Pm,_P8)))?(!B(_b9(_Pm,_cY)))?(!B(_b9(_Pm,_P7)))?E(_P6):E(_Pf):E(_Pd):E(_Pb):E(_P4);},_Pn=function(_Po,_Pp){var _Pq=E(_Pp);return (_Pq._==0)?new T1(0,function(_Ev){return new F(function(){return _sU(_Pq.a,_Po,_Ev);});}):new T1(1,function(_Ev){return new F(function(){return _sU(_Pq.a,_Po,_Ev);});});},_Pr=function(_Ps){var _Pt=E(_Ps);if(!_Pt._){return E(_OQ);}else{var _Pu=_Pt.a,_Pv=_Pt.b;if(!B(_b9(_Pu,_Ek))){var _Pw=function(_Px){if(!B(_b9(_Pu,_ON))){if(!B(_b9(_Pu,_OM))){if(!B(_b9(_Pu,_G6))){if(!B(_b9(_Pu,_G5))){var _Py=new T(function(){return B(unAppCStr("Unexpected ",new T2(1,_dA,new T(function(){return B(_ss(_Pu,_dB));}))));});return new T1(0,_Py);}else{return new T1(1,new T2(0,_OL,_Pv));}}else{return new T1(1,new T2(0,_Ov,_Pv));}}else{return new T1(1,new T2(0,_Os,_Pv));}}else{return new T1(1,new T2(0,_Os,_Pv));}},_Pz=B(_LP(_Pu));if(!_Pz._){var _PA=B(_Pw(_));if(!_PA._){return new T1(0,new T(function(){return B(_O(_OO,_PA.a));}));}else{return new F(function(){return _OV(_PA.a);});}}else{var _PB=E(_Pv);if(!_PB._){var _PC=B(_Pw(_));if(!_PC._){return new T1(0,new T(function(){return B(_O(_OO,_PC.a));}));}else{return new F(function(){return _OV(_PC.a);});}}else{var _PD=B(_sy(_On,_Pu,_PB.a));if(!_PD._){return new T1(0,new T(function(){return B(_O(_OO,_PD.a));}));}else{var _PE=B(_Pr(_PB.b));if(!_PE._){return E(_PE);}else{var _PF=new T(function(){var _PG=new T(function(){return B(A1(_Pz.a,_PD.a));}),_PH=function(_Ev){return new F(function(){return _Gd(_PG,_Ev);});};return B(_Pn(function(_PI){return new F(function(){return _du(_PH,_PI);});},_PE.a));});return new T1(1,_PF);}}}}}else{var _PJ=E(_Pv);if(!_PJ._){return E(_OU);}else{var _PK=_PJ.a,_PL=_PJ.b,_PM=B(_Pg(_PK,_bZ));if(E(_PM)==115){var _PN=E(_PK);if(!_PN._){var _PO=E(_bX);}else{var _PO=B(_bO(_PN.a,_PN.b));}var _PP=_PO;}else{var _PP=E(_PK);}if(!B(_b9(_PP,_Om))){if(!B(_b9(_PP,_Ol))){if(!B(_b9(_PP,_Ok))){if(!B(_b9(_PP,_Oj))){if(!B(_b9(_PP,_Oi))){if(!B(_b9(_PP,_Oh))){var _PQ=B(_be(_PK));if(!_PQ._){var _PR=function(_PS){if(E(_PM)==115){var _PT=E(_PK);if(!_PT._){var _PU=E(_bX);}else{var _PU=B(_bO(_PT.a,_PT.b));}var _PV=_PU;}else{var _PV=E(_PK);}if(!B(_b9(_PV,_Og))){if(!B(_b9(_PV,_Ep))){if(!B(_b9(_PV,_En))){if(!B(_b9(_PV,_G7))){if(!B(_b9(_PV,_Of))){var _PW=function(_PX){var _PY=new T(function(){return B(unAppCStr("Unexpected ",new T(function(){return B(_O(_PK,_OR));})));});return new T1(0,_PY);},_PZ=B(_qH(B(_dD(_qG,_PK))));if(!_PZ._){return new F(function(){return _PW(_);});}else{var _Q0=_PZ.a;if(!E(_PZ.b)._){var _Q1=E(_PL);if(!_Q1._){return new F(function(){return _PW(_);});}else{var _Q2=_Q1.a,_Q3=B(_Pk(_Q2));if(!_Q3._){var _Q4=new T(function(){var _Q5=new T(function(){var _Q6=new T(function(){var _Q7=new T(function(){return B(unAppCStr(", got ",new T2(1,_dA,new T(function(){return B(_ss(_Q2,_dB));}))));},1);return B(_O(B(_3e(0,E(_Q0),_u)),_Q7));});return B(unAppCStr(" after \'to\' number ",_Q6));},1);return B(_O(_Q3.a,_Q5));});return new T1(0,_Q4);}else{var _Q8=E(_Q3.a),_Q9=new T(function(){return B(_6n(_Q8.b,_Q0));});return new T1(1,new T2(0,new T1(1,function(_Ev){return new F(function(){return _HK(_Q8.a,_Q9,_Ev);});}),_Q1.b));}}}else{return new F(function(){return _PW(_);});}}}else{return new T1(1,new T2(0,_Oe,_PL));}}else{return new T1(1,new T2(0,_NW,_PL));}}else{return new T1(1,new T2(0,_ND,_PL));}}else{return new T1(1,new T2(0,_MD,_PL));}}else{return new T1(1,new T2(0,_Mk,_PL));}};if(!B(_b9(_PK,_Gb))){var _Qa=B(_PR(_));if(!_Qa._){return new T1(0,new T(function(){return B(_O(_OO,_Qa.a));}));}else{return new F(function(){return _OV(_Qa.a);});}}else{var _Qb=E(_PL);if(!_Qb._){var _Qc=B(_PR(_));if(!_Qc._){return new T1(0,new T(function(){return B(_O(_OO,_Qc.a));}));}else{return new F(function(){return _OV(_Qc.a);});}}else{var _Qd=function(_Qe){var _Qf=B(_PR(_));if(!_Qf._){return new T1(0,new T(function(){return B(_O(_OO,_Qf.a));}));}else{return new F(function(){return _OV(_Qf.a);});}},_Qg=B(_qH(B(_dD(_qG,_Qb.a))));if(!_Qg._){return new F(function(){return _Qd(_);});}else{if(!E(_Qg.b)._){var _Qh=B(_Qi(_Qb.b));if(!_Qh._){return new T1(0,_Qh.a);}else{var _Qj=new T(function(){return B(_Pn(function(_Ev){return new F(function(){return _II(_Qg.a,_Ev);});},_Qh.a));});return new T1(1,_Qj);}}else{return new F(function(){return _Qd(_);});}}}}}else{var _Qk=B(_Qi(_PL));if(!_Qk._){return new T1(0,_Qk.a);}else{var _Ql=new T(function(){return B(_Pn(function(_Ev){return new F(function(){return _II(_PQ.a,_Ev);});},_Qk.a));});return new T1(1,_Ql);}}}else{var _Qm=B(_Qi(_PL));return (_Qm._==0)?new T1(0,_Qm.a):new T1(1,new T(function(){return B(_Pn(_IP,_Qm.a));}));}}else{var _Qn=B(_Qi(_PL));return (_Qn._==0)?new T1(0,_Qn.a):new T1(1,new T(function(){return B(_Pn(_IO,_Qn.a));}));}}else{var _Qo=B(_Qi(_PL));return (_Qo._==0)?new T1(0,_Qo.a):new T1(1,new T(function(){return B(_Pn(_Ii,_Qo.a));}));}}else{var _Qp=B(_Qi(_PL));return (_Qp._==0)?new T1(0,_Qp.a):new T1(1,new T(function(){return B(_Pn(_Ih,_Qp.a));}));}}else{var _Qq=B(_Qi(_PL));return (_Qq._==0)?new T1(0,_Qq.a):new T1(1,new T(function(){return B(_Pn(_If,_Qq.a));}));}}else{var _Qr=B(_Qi(_PL));return (_Qr._==0)?new T1(0,_Qr.a):new T1(1,new T(function(){return B(_Pn(_HT,_Qr.a));}));}}}}},_OY=function(_Qs,_Qt){var _Qu=E(_Qs);if(!_Qu._){var _Qv=B(_Pr(_Qt));return (_Qv._==0)?new T1(0,_Qv.a):new T1(1,new T(function(){return B(_Pn(_Qu.a,_Qv.a));}));}else{var _Qw=B(_Qi(_Qt));return (_Qw._==0)?new T1(0,_Qw.a):new T1(1,new T(function(){return B(_Pn(_Qu.a,_Qw.a));}));}},_Qx=function(_Qy){var _Qz=E(_Qy);return new F(function(){return _OY(_Qz.a,_Qz.b);});},_QA=function(_QB){return E(_QB);},_QC=function(_QD){return new T1(1,new T(function(){return B(_QA(_QD));}));},_QE=function(_Ev){return new F(function(){return _du(_QC,_Ev);});},_QF=function(_Ev){return new F(function(){return _d6(_QE,_Ev);});},_QG=new T1(0,_QF),_QH=function(_QI,_QJ){var _QK=E(_QI);if(!_QK._){return __Z;}else{var _QL=E(_QJ);return (_QL._==0)?__Z:new T2(1,new T2(0,_QK.a,_QL.a),new T(function(){return B(_QH(_QK.b,_QL.b));}));}},_Qi=function(_QM){var _QN=E(_QM);if(!_QN._){return E(_Ga);}else{var _QO=_QN.a,_QP=_QN.b,_QQ=B(_Pk(_QO));if(!_QQ._){var _QR=function(_QS){var _QT=B(_be(_QO));if(!_QT._){var _QU=function(_QV){var _QW=function(_QX){if(!B(_b9(_QO,_Ep))){if(!B(_b9(_QO,_Eo))){if(!B(_b9(_QO,_En))){if(!B(_b9(_QO,_Ej))){var _QY=function(_QZ){if(!B(_b9(_QO,_Em))){if(!B(_b9(_QO,_El))){var _R0=function(_R1){var _R2=function(_R3){var _R4=function(_R5){if(!B(_b9(_QO,_EI))){var _R6=function(_R7){var _R8=B(_bu(_QO));if(!_R8._){if(!B(_b9(_QO,_G6))){if(!B(_b9(_QO,_G5))){var _R9=new T(function(){return B(unAppCStr("Unexpected ",new T2(1,_dA,new T(function(){return B(_ss(_QO,_dB));}))));});return new T1(0,_R9);}else{return new T1(1,new T2(0,_G4,_QP));}}else{return new T1(1,new T2(0,_EU,_QP));}}else{var _Ra=function(_Ev){return new F(function(){return _du(_R8.a,_Ev);});};return new T1(1,new T2(0,new T1(1,function(_Ev){return new F(function(){return _Ew(_Ra,_Ev);});}),_QP));}};if(!B(_b9(_QO,_EH))){return new F(function(){return _R6(_);});}else{var _Rb=E(_QP);if(!_Rb._){return new F(function(){return _R6(_);});}else{var _Rc=E(_Rb.b);if(!_Rc._){return new F(function(){return _R6(_);});}else{var _Rd=_Rc.a,_Re=E(_Rc.b);if(!_Re._){return new F(function(){return _R6(_);});}else{if(!B(_b9(_Rd,_Ek))){var _Rf=new T(function(){return B(unAppCStr("Translate syntax should be \'translate _ to _\', got ",new T2(1,_dA,new T(function(){return B(_ss(_Rd,_dB));}))));});return new T1(0,_Rf);}else{var _Rg=new T(function(){var _Rh=new T(function(){var _Ri=E(_Re.a);if(!_Ri._){return E(_EG);}else{var _Rj=_Ri.a,_Rk=E(_Ri.b);if(!_Rk._){var _Rl=new T(function(){return new T2(1,_Rj,_Rl);});return E(_Rl);}else{return new T2(1,_Rj,new T(function(){return B(_ar(_Rk.a,_Rk.b));}));}}},1);return B(_ab(B(_QH(_Rb.a,_Rh))));}),_Rm=function(_Rn){var _Ro=E(_Rn),_Rp=B(_aN(_Ro,_Rg));return (_Rp._==0)?E(_Ro):E(_Rp.a);},_Rq=function(_Ev){return new F(function(){return _du(_Rm,_Ev);});};return new T1(1,new T2(0,new T1(1,function(_Ev){return new F(function(){return _Ew(_Rq,_Ev);});}),_Re.b));}}}}}}else{return new T1(1,new T2(0,_EE,_QP));}},_Rr=B(_tb(_QO));if(!_Rr._){return new F(function(){return _R4(_);});}else{var _Rs=E(_QP);if(!_Rs._){return new F(function(){return _R4(_);});}else{var _Rt=_Rs.a,_Ru=B(_cZ(_Rt));if(!_Ru._){var _Rv=new T(function(){var _Rw=new T(function(){return B(_O(_QO,new T(function(){return B(unAppCStr("\', got ",_Rt));},1)));});return B(unAppCStr("Expecting character class after \'",_Rw));});return new T1(0,_Rv);}else{var _Rx=new T(function(){return B(A1(_Rr.a,_Ru.a));}),_Ry=function(_Ev){return new F(function(){return _sK(_Rx,_Ev);});};return new T1(1,new T2(0,new T1(1,function(_Ev){return new F(function(){return _Ew(_Ry,_Ev);});}),_Rs.b));}}}};if(!B(_b9(_QO,_Ek))){return new F(function(){return _R2(_);});}else{var _Rz=E(_QP);if(!_Rz._){return new F(function(){return _R2(_);});}else{if(!B(_b9(_Rz.a,_Ej))){return new F(function(){return _R2(_);});}else{return new T1(1,new T2(0,_Ei,_Rz.b));}}}};if(!B(_b9(_QO,_DK))){return new F(function(){return _R0(_);});}else{var _RA=E(_QP);if(!_RA._){return new F(function(){return _R0(_);});}else{var _RB=B(_sy(_Dp,_DK,_RA.a));if(!_RB._){return new T1(0,_RB.a);}else{var _RC=function(_RD){return new F(function(){return _6q(_RD,_RB.a);});},_RE=function(_RF){var _RG=E(_RF);if(!_RG._){return E(_RG);}else{var _RH=new T(function(){return B(_du(function(_RI){return new F(function(){return _aE(_RC,E(_RI));});},_RG.a));});return new T1(1,_RH);}};return new T1(1,new T2(0,new T1(1,function(_RJ){return new F(function(){return _du(_RE,_RJ);});}),_RA.b));}}}}else{return new T1(1,new T2(0,_DJ,_QP));}}else{return new T1(1,new T2(0,_Dz,_QP));}};if(!B(_b9(_QO,_Do))){return new F(function(){return _QY(_);});}else{var _RK=E(_QP);if(!_RK._){return new F(function(){return _QY(_);});}else{var _RL=B(_sy(_Dp,_Do,_RK.a));if(!_RL._){return new T1(0,_RL.a);}else{var _RM=function(_RN){var _RO=E(_RN);if(!_RO._){return E(_RO);}else{var _RP=new T(function(){return B(_du(function(_RQ){var _RR=E(_RQ);if(33>_RR){return E(_RR);}else{if(_RR>126){return E(_RR);}else{var _RS=33+B(_ax((_RR-33|0)+E(_RL.a)|0,94))|0;if(_RS>>>0>1114111){return new F(function(){return _av(_RS);});}else{return _RS;}}}},_RO.a));});return new T1(1,_RP);}};return new T1(1,new T2(0,new T1(1,function(_RT){return new F(function(){return _du(_RM,_RT);});}),_RK.b));}}}}else{return new T1(1,new T2(0,_Dn,_QP));}}else{return new T1(1,new T2(0,_yc,_QP));}}else{return new T1(1,new T2(0,_w4,_QP));}}else{return new T1(1,new T2(0,_vU,_QP));}};if(E(B(_Pg(_QO,_bZ)))==115){var _RU=E(_QO);if(!_RU._){return E(_bX);}else{if(!B(_b9(B(_bO(_RU.a,_RU.b)),_G7))){return new F(function(){return _QW(_);});}else{return new T1(1,new T2(0,_QG,_QP));}}}else{if(!B(_b9(_QO,_G7))){return new F(function(){return _QW(_);});}else{return new T1(1,new T2(0,_QG,_QP));}}};if(!B(_b9(_QO,_Gb))){return new F(function(){return _QU(_);});}else{var _RV=E(_QP);if(!_RV._){return new F(function(){return _QU(_);});}else{var _RW=B(_sy(_Gc,_Gb,_RV.a));return (_RW._==0)?new T1(0,_RW.a):new T1(1,new T2(0,new T1(0,function(_Ev){return new F(function(){return _uI(_RW.a,_Ev);});}),_RV.b));}}}else{return new T1(1,new T2(0,new T1(0,function(_Ev){return new F(function(){return _uI(_QT.a,_Ev);});}),_QP));}},_RX=function(_RY){var _RZ=B(_QR(_));if(!_RZ._){return new T1(0,new T(function(){return B(_O(_G8,_RZ.a));}));}else{return new F(function(){return _Qx(_RZ.a);});}},_S0=B(_qH(B(_dD(_qG,_QO))));if(!_S0._){return new F(function(){return _RX(_);});}else{var _S1=_S0.a;if(!E(_S0.b)._){var _S2=E(_QP);if(!_S2._){var _S3=B(_QR(_));if(!_S3._){return new T1(0,new T(function(){return B(_O(_G8,_S3.a));}));}else{return new F(function(){return _Qx(_S3.a);});}}else{var _S4=_S2.a,_S5=B(_Pk(_S4));if(!_S5._){var _S6=new T(function(){var _S7=new T(function(){var _S8=new T(function(){var _S9=new T(function(){var _Sa=new T(function(){return B(unAppCStr(", got ",new T2(1,_dA,new T(function(){return B(_ss(_S4,_dB));}))));},1);return B(_O(B(_3e(0,E(_S1),_u)),_Sa));});return B(unAppCStr(" after number ",_S9));},1);return B(_O(_S5.a,_S8));},1);return B(_O(_G8,_S7));});return new T1(0,_S6);}else{var _Sb=E(_S5.a),_Sc=B(_Pr(_S2.b));if(!_Sc._){return new T1(0,_Sc.a);}else{var _Sd=new T(function(){var _Se=new T(function(){return B(_6n(_Sb.b,_S1));}),_Sf=function(_Ev){return new F(function(){return _vw(_Sb.a,_Se,_Ev);});};return B(_Pn(function(_Ev){return new F(function(){return _d6(_Sf,_Ev);});},_Sc.a));});return new T1(1,_Sd);}}}}else{return new F(function(){return _RX(_);});}}}else{var _Sg=E(_QQ.a),_Sh=B(_Pr(_QP));if(!_Sh._){return new T1(0,_Sh.a);}else{var _Si=new T(function(){var _Sj=function(_Sk){var _Sl=E(_Sk);if(!_Sl._){return __Z;}else{var _Sm=_Sl.b,_Sn=E(_Sl.a);if(!_Sn._){return new T2(1,new T1(0,_Sn.a),new T(function(){return B(_Sj(_Sm));}));}else{return new F(function(){return _O(B(_vw(_Sg.a,_Sg.b,_Sn.a)),new T(function(){return B(_Sj(_Sm));},1));});}}};return B(_Pn(function(_So){return new F(function(){return _Sj(B(_dy(_So)));});},_Sh.a));});return new T1(1,_Si);}}}},_Sp=function(_Sq){var _Sr=B(_Qi(_Sq));return (_Sr._==0)?new T1(0,_Sr.a):new T1(1,function(_Ss){var _St=E(_Sr.a);if(!_St._){return new F(function(){return _6e(_61,_St.a,_Ss);});}else{return new F(function(){return _6e(_5W,_St.a,_Ss);});}});},_Su=new T(function(){return eval("document.body");}),_Sv="value",_Sw=new T(function(){return eval("(function(e,p){var x = e[p];return typeof x === \'undefined\' ? \'\' : x.toString();})");}),_Sx=function(_Sy){return E(E(_Sy).a);},_Sz=function(_SA){return E(E(_SA).b);},_SB=function(_SC){return new F(function(){return fromJSStr(E(_SC));});},_SD=function(_SE){return E(E(_SE).a);},_SF=function(_SG){return E(E(_SG).b);},_SH=function(_SI,_SJ,_SK,_SL){var _SM=new T(function(){var _SN=function(_){var _SO=__app2(E(_Sw),B(A2(_SD,_SI,_SK)),E(_SL));return new T(function(){return String(_SO);});};return E(_SN);});return new F(function(){return A2(_SF,_SJ,_SM);});},_SP=function(_SQ){return E(E(_SQ).d);},_SR=function(_SS,_ST,_SU,_SV){var _SW=B(_Sx(_ST)),_SX=new T(function(){return B(_SP(_SW));}),_SY=function(_SZ){return new F(function(){return A1(_SX,new T(function(){return B(_SB(_SZ));}));});},_T0=new T(function(){return B(_SH(_SS,_ST,_SU,new T(function(){return toJSStr(E(_SV));},1)));});return new F(function(){return A3(_Sz,_SW,_T0,_SY);});},_T1=new T(function(){return eval("(function(e,p,v){e[p] = v;})");}),_T2=new T(function(){return B(unCStr("textContent"));}),_T3=new T(function(){return B(unCStr("innerHTML"));}),_T4=new T(function(){return B(unCStr("checked"));}),_T5=new T(function(){return B(unCStr("false"));}),_T6=new T(function(){return B(unCStr("value"));}),_T7=new T(function(){return B(unCStr("err"));}),_T8=function(_T9){return E(E(_T9).a);},_Ta=function(_Tb){return E(E(_Tb).b);},_Tc=function(_Td){return E(E(_Td).a);},_Te=function(_){return new F(function(){return nMV(_2a);});},_Tf=new T(function(){return B(_45(_Te));}),_Tg=new T(function(){return eval("(function(e,name,f){e.addEventListener(name,f,false);return [f];})");}),_Th=function(_Ti){return E(E(_Ti).b);},_Tj=function(_Tk,_Tl,_Tm,_Tn,_To,_Tp){var _Tq=B(_T8(_Tk)),_Tr=B(_Sx(_Tq)),_Ts=new T(function(){return B(_SF(_Tq));}),_Tt=new T(function(){return B(_SP(_Tr));}),_Tu=new T(function(){return B(A2(_SD,_Tl,_Tn));}),_Tv=new T(function(){return B(A2(_Tc,_Tm,_To));}),_Tw=function(_Tx){return new F(function(){return A1(_Tt,new T3(0,_Tv,_Tu,_Tx));});},_Ty=function(_Tz){var _TA=new T(function(){var _TB=new T(function(){var _TC=__createJSFunc(2,function(_TD,_){var _TE=B(A2(E(_Tz),_TD,_));return _49;}),_TF=_TC;return function(_){return new F(function(){return __app3(E(_Tg),E(_Tu),E(_Tv),_TF);});};});return B(A1(_Ts,_TB));});return new F(function(){return A3(_Sz,_Tr,_TA,_Tw);});},_TG=new T(function(){var _TH=new T(function(){return B(_SF(_Tq));}),_TI=function(_TJ){var _TK=new T(function(){return B(A1(_TH,function(_){var _=wMV(E(_Tf),new T1(1,_TJ));return new F(function(){return A(_Ta,[_Tm,_To,_TJ,_]);});}));});return new F(function(){return A3(_Sz,_Tr,_TK,_Tp);});};return B(A2(_Th,_Tk,_TI));});return new F(function(){return A3(_Sz,_Tr,_TG,_Ty);});},_TL=new T(function(){return eval("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})");}),_TM=function(_TN,_TO){var _TP=E(_TO);if(!_TP._){return new T2(0,_u,_u);}else{var _TQ=_TP.a;if(!B(A1(_TN,_TQ))){var _TR=new T(function(){var _TS=B(_TM(_TN,_TP.b));return new T2(0,_TS.a,_TS.b);});return new T2(0,new T2(1,_TQ,new T(function(){return E(E(_TR).a);})),new T(function(){return E(E(_TR).b);}));}else{return new T2(0,_u,_TP);}}},_TT=function(_TU,_TV){while(1){var _TW=E(_TV);if(!_TW._){return __Z;}else{if(!B(A1(_TU,_TW.a))){return E(_TW);}else{_TV=_TW.b;continue;}}}},_TX=function(_TY){var _TZ=B(_TT(_c5,_TY));if(!_TZ._){return __Z;}else{var _U0=new T(function(){var _U1=B(_TM(_c5,_TZ));return new T2(0,_U1.a,_U1.b);});return new T2(1,new T(function(){return E(E(_U0).a);}),new T(function(){return B(_TX(E(_U0).b));}));}},_U2=function(_U3,_){var _U4=E(_U3);if(!_U4._){return new F(function(){return _5B(_);});}else{var _U5=_U4.a,_U6=E(_U4.b);if(!_U6._){return new F(function(){return _5B(_);});}else{var _U7=_U6.a,_U8=E(_U6.b);if(!_U8._){return new F(function(){return _5B(_);});}else{var _U9=_U8.a,_Ua=E(_U8.b);if(!_Ua._){return new F(function(){return _5B(_);});}else{var _Ub=E(_Ua.b);if(!_Ub._){return new F(function(){return _5B(_);});}else{var _Uc=E(_Ub.b);if(!_Uc._){return new F(function(){return _5B(_);});}else{var _Ud=_Uc.a;if(!E(_Uc.b)._){var _Ue=function(_){var _Uf=__app2(E(_Sw),E(_U5),E(_Sv));return new T1(1,new T(function(){var _Ug=String(_Uf);return fromJSStr(_Ug);}));},_Uh=B(_Ue(_)),_Ui=function(_Uj,_){var _Uk=__app2(E(_Sw),E(_U7),E(_Sv)),_Ul=E(_Uj);if(!_Ul._){return _2t;}else{var _Um=B(_Sp(B(_TX(_Ul.a))));if(!_Um._){var _Un=__app3(E(_TL),E(_Su),toJSStr(E(_T7)),true),_Uo=__app3(E(_T1),E(_U9),toJSStr(E(_T2)),toJSStr(E(_Um.a)));return new F(function(){return _4B(_);});}else{var _Up=__app3(E(_TL),E(_Su),toJSStr(E(_T7)),false),_Uq=__app3(E(_T1),E(_U9),toJSStr(E(_T3)),toJSStr(B(A1(_Um.a,new T(function(){var _Ur=String(_Uk);return fromJSStr(_Ur);})))));return new F(function(){return _4B(_);});}}},_Us=B(_Ui(_Uh,_)),_Ut=B(A(_Tj,[_4F,_4E,_4A,_Ua.a,_5D,function(_Uu,_){var _Uv=B(_Ue(_));return new F(function(){return _Ui(_Uv,_);});},_])),_Uw=new T(function(){return B(_SR(_4E,_2s,_Ub.a,_T4));}),_Ux=function(_Uy,_){var _Uz=E(_Uy);if(!_Uz._){return _2t;}else{if(!B(_b9(B(_du(_bg,_Uz)),_T5))){var _UA=B(_Ue(_));return new F(function(){return _Ui(_UA,_);});}else{return _2t;}}},_UB=B(A(_Tj,[_4F,_4E,_34,_U5,_5E,function(_UC,_){var _UD=B(A1(_Uw,_));return new F(function(){return _Ux(_UD,_);});},_])),_UE=B(A(_Tj,[_4F,_4E,_34,_U7,_5E,function(_UF,_){var _UG=B(A1(_Uw,_));return new F(function(){return _Ux(_UG,_);});},_])),_UH=B(A(_Tj,[_4F,_4E,_2H,_Ud,_5C,function(_UI,_){var _UJ=__app2(E(_Sw),E(_Ud),E(_Sv)),_UK=String(_UJ),_UL=__app3(E(_T1),E(_U5),toJSStr(E(_T6)),toJSStr(fromJSStr(_UK))),_UM=B(A1(_Uw,_));return new F(function(){return _Ux(_UM,_);});},_]));return _2t;}else{return new F(function(){return _5B(_);});}}}}}}}},_UN=new T(function(){return B(unCStr("args"));}),_UO=new T(function(){return B(unCStr("input"));}),_UP=new T(function(){return B(unCStr("output"));}),_UQ=new T(function(){return B(unCStr("go"));}),_UR=new T(function(){return B(unCStr("auto"));}),_US=new T(function(){return B(unCStr("try"));}),_UT=new T2(1,_US,_u),_UU=new T2(1,_UR,_UT),_UV=new T2(1,_UQ,_UU),_UW=new T2(1,_UP,_UV),_UX=new T2(1,_UO,_UW),_UY=new T2(1,_UN,_UX),_UZ=function(_V0){return new F(function(){return toJSStr(E(_V0));});},_V1=new T(function(){return B(_du(_UZ,_UY));}),_V2=new T(function(){return eval("(function(id){return document.getElementById(id);})");}),_V3=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_V4=new T(function(){return B(err(_V3));}),_V5=function(_V6){var _V7=E(_V6);return (_V7._==0)?E(_V4):E(_V7.a);},_V8=function(_V9,_Va){while(1){var _Vb=B((function(_Vc,_Vd){var _Ve=E(_Vc);if(!_Ve._){return __Z;}else{var _Vf=_Ve.b,_Vg=E(_Vd);if(!_Vg._){return __Z;}else{var _Vh=_Vg.b;if(!E(_Vg.a)._){return new T2(1,_Ve.a,new T(function(){return B(_V8(_Vf,_Vh));}));}else{_V9=_Vf;_Va=_Vh;return __continue;}}}})(_V9,_Va));if(_Vb!=__continue){return _Vb;}}},_Vi=new T(function(){return B(unAppCStr("[]",_u));}),_Vj=new T2(1,_1Q,_u),_Vk=function(_Vl){var _Vm=E(_Vl);if(!_Vm._){return E(_Vj);}else{var _Vn=new T(function(){return B(_O(fromJSStr(E(_Vm.a)),new T(function(){return B(_Vk(_Vm.b));},1)));});return new T2(1,_1P,_Vn);}},_Vo=function(_Vp,_Vq){var _Vr=new T(function(){var _Vs=B(_V8(_Vp,_Vq));if(!_Vs._){return E(_Vi);}else{var _Vt=new T(function(){return B(_O(fromJSStr(E(_Vs.a)),new T(function(){return B(_Vk(_Vs.b));},1)));});return new T2(1,_1R,_Vt);}});return new F(function(){return err(B(unAppCStr("Elements with the following IDs could not be found: ",_Vr)));});},_Vu=function(_Vv){while(1){var _Vw=E(_Vv);if(!_Vw._){return false;}else{if(!E(_Vw.a)._){return true;}else{_Vv=_Vw.b;continue;}}}},_Vx=function(_Vy,_Vz,_VA){var _VB=B(_Sx(_Vy)),_VC=function(_VD){if(!B(_Vu(_VD))){return new F(function(){return A1(_VA,new T(function(){return B(_du(_V5,_VD));}));});}else{return new F(function(){return _Vo(_Vz,_VD);});}},_VE=new T(function(){var _VF=new T(function(){return B(A2(_SP,_VB,_u));}),_VG=function(_VH){var _VI=E(_VH);if(!_VI._){return E(_VF);}else{var _VJ=new T(function(){return B(_VG(_VI.b));}),_VK=function(_VL){return new F(function(){return A3(_Sz,_VB,_VJ,function(_VM){return new F(function(){return A2(_SP,_VB,new T2(1,_VL,_VM));});});});},_VN=new T(function(){var _VO=function(_){var _VP=__app1(E(_V2),E(_VI.a)),_VQ=__eq(_VP,E(_49));return (E(_VQ)==0)?new T1(1,_VP):_2a;};return B(A2(_SF,_Vy,_VO));});return new F(function(){return A3(_Sz,_VB,_VN,_VK);});}};return B(_VG(_Vz));});return new F(function(){return A3(_Sz,_VB,_VE,_VC);});},_VR=new T(function(){return B(_Vx(_2s,_V1,_U2));});
var hasteMain = function() {B(A(_VR, [0]));};window.onload = hasteMain;