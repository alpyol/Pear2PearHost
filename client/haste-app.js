// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
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
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
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
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
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

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
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
    return [0, sign, manHigh, manLow, exp];
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
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
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
    mv.x = x[1];
    return x[2];
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
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
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

function jsGetMouseCoords(e) {
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

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
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
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
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
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
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

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
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

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

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
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

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
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

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

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
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

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
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
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

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

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

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

var _0=function(_1,_2){return new F(function(){return A(_2,[_1]);});},_3=function(_4){var _5=jsRound(_4),_6=_5;return [0,_6>>>0&255];},_7=function(_8){var _9=B(A(_8,[_])),_a=_9;return E(_a);},_b=function(_c){return new F(function(){return _7(function(_){var _=0;return new F(function(){return eval(_c);});});});},_d=new T(function(){return B(_b("(function(b,i){return b.getUint8(i);})"));}),_e=function(_f){return function(_g,_){var _h=B(A(new T(function(){return B(A(_d,[E(_f)]));}),[E(E(_g)[1]),_])),_i=_h;return new T(function(){return B(_3(_i));});};},_j=new T(function(){return B(unCStr("Wrong magic byte for ServerException"));}),_k=[0,_j],_l=function(_m,_n){var _o=E(_m);return _o[0]==0?E(_n):[1,_o[1],new T(function(){return B(_l(_o[2],_n));})];},_p=function(_q,_r){var _s=jsShowI(_q),_t=_s;return new F(function(){return _l(fromJSStr(_t),_r);});},_u=[0,41],_v=[0,40],_w=function(_x,_y,_z){if(_y>=0){return new F(function(){return _p(_y,_z);});}else{return _x<=6?B(_p(_y,_z)):[1,_v,new T(function(){var _A=jsShowI(_y),_B=_A;return B(_l(fromJSStr(_B),[1,_u,_z]));})];}},_C=[0],_D=function(_E){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_w(9,_E,_C));}))));});},_F=function(_G){var _H=jsRound(_G),_I=_H;return [0,_I];},_J=new T(function(){return B(_b("(function(b,i){return b.getInt32(i,true);})"));}),_K=function(_L){return function(_M,_){var _N=B(A(new T(function(){return B(A(_J,[E(_L)]));}),[E(E(_M)[1]),_])),_O=_N;return new T(function(){return B(_F(_O));});};},_P=function(_Q,_R){return [1,[0,new T(function(){return [0,E(_R)[1]+4|0];}),new T(function(){var _S=B(_7(function(_){var _=0;return new F(function(){return A(_K,[_Q,_R,_]);});}))[1];if(_S>>>0>1114111){var _T=B(_D(_S));}else{var _T=[0,_S];}var _U=_T,_V=_U,_W=_V;return _W;})]];},_X=[0,_P],_Y=0,_Z=new T(function(){return [0,"(function(a,x) {a.push(x);})"];}),_10=function(_11,_12){return [0,_11,_12];},_13=function(_14){return E(_14);},_15=new T(function(){return B(_10(_13,_13));}),_16=[0,4],_17=new T(function(){return [0,"Int32Array"];}),_18=function(_19){return E(E(_19)[2]);},_1a=new T(function(){return [0,"window[\'toABle\']"];}),_1b=new T(function(){return B(_b(E(_1a)[1]));}),_1c=function(_1d,_1e){return function(_1f){return function(_1g,_){return new F(function(){return A(new T(function(){return B(A(new T(function(){return B(A(_1b,[E(E(_1e)[1])]));}),[E(E(_1f)[1])]));}),[B(A(new T(function(){return B(_18(_1d));}),[_1g])),_]);});};};},_1h=function(_1i){return function(_1j,_){var _1k=B(A(_b,[E(_Z)[1],E(_1j),E(new T(function(){return B(_7(function(_){var _=0;return new F(function(){return A(_1c,[_15,_17,_16,new T(function(){return E(E(_1i)[1]);}),_]);});}));})),_])),_1l=_1k;return _Y;};},_1m=function(_1n){return [0,B(_1h(new T(function(){return [0,E(_1n)[1]];})))];},_1o=[0,_X,_1m],_1p=function(_1q,_1r){return [1,[0,_1r,_C]];},_1s=[0,1],_1t=function(_1u){return I_toInt(_1u)>>>0;},_1v=function(_1w){var _1x=E(_1w);return _1x[0]==0?_1x[1]>>>0:B(_1t(_1x[1]));},_1y=function(_1z){return [0,B(_1v(_1z))];},_1A=function(_1B,_1C){return [1,new T(function(){return B(_1y(_1B));}),_1C];},_1D=function(_1E){return [0,_1E];},_1F=function(_1G){return [1,I_fromInt(_1G)];},_1H=function(_1I){var _1J=_1I&4294967295;return _1J<0?B(_1F(_1I)):B(_1D(_1J));},_1K=[0,0],_1L=function(_1M,_1N){var _1O=E(_1M);if(!_1O[0]){var _1P=_1O[1],_1Q=E(_1N);return _1Q[0]==0?_1P>=_1Q[1]:I_compareInt(_1Q[1],_1P)<=0;}else{var _1R=_1O[1],_1S=E(_1N);return _1S[0]==0?I_compareInt(_1R,_1S[1])>=0:I_compare(_1R,_1S[1])>=0;}},_1T=function(_1U,_1V){var _1W=E(_1U);if(!_1W[0]){var _1X=_1W[1],_1Y=E(_1V);return _1Y[0]==0?_1X>_1Y[1]:I_compareInt(_1Y[1],_1X)<0;}else{var _1Z=_1W[1],_20=E(_1V);return _20[0]==0?I_compareInt(_1Z,_20[1])>0:I_compare(_1Z,_20[1])>0;}},_21=function(_22,_23){var _24=E(_22);if(!_24[0]){var _25=_24[1],_26=E(_23);return _26[0]==0?_25<_26[1]:I_compareInt(_26[1],_25)>0;}else{var _27=_24[1],_28=E(_23);return _28[0]==0?I_compareInt(_27,_28[1])<0:I_compare(_27,_28[1])<0;}},_29=function(_2a,_2b){while(1){var _2c=E(_2a);if(!_2c[0]){var _2d=_2c[1],_2e=E(_2b);if(!_2e[0]){var _2f=_2e[1],_2g=addC(_2d,_2f);if(!E(_2g[2])){return [0,_2g[1]];}else{_2a=[1,I_fromInt(_2d)];_2b=[1,I_fromInt(_2f)];continue;}}else{_2a=[1,I_fromInt(_2d)];_2b=_2e;continue;}}else{var _2h=E(_2b);if(!_2h[0]){_2a=_2c;_2b=[1,I_fromInt(_2h[1])];continue;}else{return [1,I_add(_2c[1],_2h[1])];}}}},_2i=function(_2j,_2k,_2l,_2m,_2n){if(!B(_1L(_2m,_1K))){var _2o=function(_2p){if(!B(_21(_2p,_2n))){return new F(function(){return A(_2j,[_2p,new T(function(){return B(_2o(B(_29(_2p,_2m))));})]);});}else{return E(_2k);}};return new F(function(){return _2o(_2l);});}else{var _2q=function(_2r){if(!B(_1T(_2r,_2n))){return new F(function(){return A(_2j,[_2r,new T(function(){return B(_2q(B(_29(_2r,_2m))));})]);});}else{return E(_2k);}};return new F(function(){return _2q(_2l);});}},_2s=function(_2t,_2u){return new F(function(){return _2i(_1A,_C,B(_1H(_2t)),_1s,B(_1H(_2u)));});},_2v=function(_2w){return E(E(_2w)[1]);},_2x=function(_2y){var _2z=jsRound(_2y),_2A=_2z;return [0,_2A>>>0];},_2B=new T(function(){return B(_b("(function(b,i){return b.getUint32(i,true);})"));}),_2C=function(_2D){return function(_2E,_){var _2F=B(A(new T(function(){return B(A(_2B,[E(_2D)]));}),[E(E(_2E)[1]),_])),_2G=_2F;return new T(function(){return B(_2x(_2G));});};},_2H=function(_2I){return function(_2J,_2K){var _2L=B(_2s(1,B(_7(function(_){var _=0;return new F(function(){return A(_2C,[_2J,_2K,_]);});}))[1]));if(!_2L[0]){return [1,[0,new T(function(){return [0,E(_2K)[1]+4|0];}),_C]];}else{var _2M=E(new T(function(){return B(_2v(_2I));}))[1],_2N=B(A(_2M,[_2J,new T(function(){return [0,E(_2K)[1]+4|0];})]));if(!_2N[0]){return [0,_2N[1]];}else{var _2O=E(_2N[1]),_2P=function(_2Q){var _2R=E(_2Q);return _2R[0]==0?_1p:function(_2S,_2T){var _2U=B(A(_2M,[_2S,_2T]));if(!_2U[0]){return [0,_2U[1]];}else{var _2V=E(_2U[1]),_2W=B(A(E(new T(function(){return [0,B(_2P(_2R[2]))];}))[1],[_2S,_2V[1]]));if(!_2W[0]){return E(_2W);}else{var _2X=E(_2W[1]);return [1,[0,_2X[1],[1,_2V[2],_2X[2]]]];}}};},_2Y=B(A(B(_2P(_2L[2])),[_2J,_2O[1]]));if(!_2Y[0]){return E(_2Y);}else{var _2Z=E(_2Y[1]);return [1,[0,_2Z[1],[1,_2O[2],_2Z[2]]]];}}}};},_30=new T(function(){return [0,function(_31,_32){var _33=B(A(B(_2H(_1o)),[_31,_32]));return _33[0]==0?[0,_33[1]]:[1,new T(function(){var _34=E(_33[1]);return [0,_34[1],[0,_34[2]]];})];}];}),_35=function(_36,_37){if(E(B(_7(function(_){var _=0;return new F(function(){return A(_e,[_36,_37,_]);});}))[1])==2){return new F(function(){return A(E(_30)[1],[_36,new T(function(){return [0,E(_37)[1]+1|0];})]);});}else{return E(_k);}},_38=[0,_35],_39=function(_3a,_3b){return [0,E(_3a)[1]+E(_3b)[1]|0];},_3c=function(_3d){var _3e=jsRound(_3d),_3f=_3e;return [0,_3f];},_3g=new T(function(){return B(_b("(function(b){return b.size;})"));}),_3h=new T(function(){return B(_b("(function(b){try {return new Blob([b]);} catch (e) {return new Blob([b.buffer]);}})"));}),_3i=new T(function(){return [0,"(function(b,off,len){return b.slice(off,len);})"];}),_3j=function(_3k,_3l,_3m){if(!E(_3k)){var _3n=B(_7(function(_){var _=0;return new F(function(){return A(_3h,[E(_3m),_]);});}));if(B(_7(function(_){var _=0,_3o=B(A(_3g,[E(_3n),_])),_3p=_3o;return new T(function(){return B(_3c(_3p));});}))[1]<=_3l){return E(_3n);}else{return new F(function(){return _7(function(_){var _=0;return new F(function(){return A(_b,[E(_3i)[1],E(_3n),E(0),E(_3l),_]);});});});}}else{return new F(function(){return _7(function(_){var _=0;return new F(function(){return A(_b,[E(_3i)[1],B(_7(function(_){var _=0;return new F(function(){return A(_3h,[E(_3m),_]);});})),E(_3k),E(_3k+_3l|0),_]);});});});}},_3q=function(_3r,_3s){var _3t=new T(function(){return [0,B(_7(function(_){var _=0;return new F(function(){return A(_K,[_3r,_3s,_]);});}))[1]];}),_3u=new T(function(){return [0,E(_3s)[1]+4|0];});return [1,[0,new T(function(){return B(_39(_3u,_3t));}),new T(function(){return B(_3j(E(_3u)[1],E(_3t)[1],_3r));})]];},_3v=[0,_3q],_3w=new T(function(){return B(unCStr("Wrong magic byte for ServerReply"));}),_3x=[0,_3w],_3y=function(_3z,_3A){if(E(B(_7(function(_){var _=0;return new F(function(){return A(_e,[_3z,_3A,_]);});}))[1])==1){var _3B=new T(function(){return [0,E(_3A)[1]+1|0];}),_3C=B(A(E(_3v)[1],[_3z,new T(function(){return [0,E(_3B)[1]+4|0];})]));if(!_3C[0]){return [0,_3C[1]];}else{var _3D=E(_3C[1]);return [1,[0,_3D[1],[0,new T(function(){return [0,B(_7(function(_){var _=0;return new F(function(){return A(_K,[_3z,_3B,_]);});}))[1]];}),_3D[2]]]];}}else{return E(_3x);}},_3E=[0,_3y],_3F=[0,0],_3G=new T(function(){return B(_b("(function(b,cb){var r=new FileReader();r.onload=function(){B(A(cb,[new DataView(r.result),0]));};r.readAsArrayBuffer(b);})"));}),_3H=[2],_3I=function(_3J){return [2];},_3K=function(_3L,_3M,_3N){return function(_){var _3O=E(_3L)[1],_3P=rMV(_3O),_3Q=_3P,_3R=E(_3Q);if(!_3R[0]){var _=wMV(_3O,[0,_3R[1],new T(function(){return B(_l(_3R[2],[1,[0,_3M,function(_3S){return E(new T(function(){return B(A(_3N,[_Y]));}));}],_C]));})]);return _3H;}else{var _3T=E(_3R[1]);if(!_3T[0]){var _=wMV(_3O,[0,_3M,_C]);return new T(function(){return B(A(_3N,[_Y]));});}else{var _=wMV(_3O,[1,_3T[2]]);return [1,[1,new T(function(){return B(A(_3N,[_Y]));}),[1,new T(function(){return B(A(_3T[1],[_3M,_3I]));}),_C]]];}}};},_3U=[1,_C],_3V=function(_3W,_3X){return function(_){var _3Y=E(_3W)[1],_3Z=rMV(_3Y),_40=_3Z,_41=E(_40);if(!_41[0]){var _42=_41[1],_43=E(_41[2]);if(!_43[0]){var _=wMV(_3Y,_3U);return new T(function(){return B(A(_3X,[_42]));});}else{var _44=E(_43[1]),_=wMV(_3Y,[0,_44[1],_43[2]]);return [1,[1,new T(function(){return B(A(_3X,[_42]));}),[1,new T(function(){return B(A(_44[2],[_3I]));}),_C]]];}}else{var _=wMV(_3Y,[1,new T(function(){return B(_l(_41[1],[1,function(_45){return function(_46){return E(new T(function(){return B(A(_3X,[_45]));}));};},_C]));})]);return _3H;}};},_47=function(_48,_){while(1){var _49=E(_48);if(!_49[0]){return _Y;}else{var _4a=_49[2],_4b=E(_49[1]);switch(_4b[0]){case 0:var _4c=B(A(_4b[1],[_])),_4d=_4c;_48=B(_l(_4a,[1,_4d,_C]));continue;case 1:_48=B(_l(_4a,_4b[1]));continue;default:_48=_4a;continue;}}}},_4e=function(_4f,_4g,_){var _4h=E(_4f);switch(_4h[0]){case 0:var _4i=B(A(_4h[1],[_])),_4j=_4i;return new F(function(){return _47(B(_l(_4g,[1,_4j,_C])),_);});break;case 1:return new F(function(){return _47(B(_l(_4g,_4h[1])),_);});break;default:return new F(function(){return _47(_4g,_);});}},_4k=function(_4l){return function(_4m){return [0,function(_){var _4n=nMV(_3U),_4o=_4n,_4p=[0,_4o];return [0,function(_){var _4q=B(A(_3G,[E(_4l),function(_4r,_){return new F(function(){return _4e(new T(function(){return [0,B(_3K(_4p,[0,_3F,new T(function(){return B(_7(function(_){var _=0,_4s=B(A(_3g,[E(_4l),_])),_4t=_4s;return new T(function(){return B(_3c(_4t));});}));}),_4r],_3I))];}),_C,_);});},_])),_4u=_4q;return new T(function(){return [0,B(_3V(_4p,_4m))];});}];}];};},_4v=new T(function(){return B(_b("(function(url, cb, f, err) {var ws = new WebSocket(url);ws.binaryType = \'blob\';ws.onmessage = function(e) {B(A(cb,[ws,e.data,0]));};ws.onopen = function(e) {B(A(f,[ws,0]));};ws.onerror = function(e) {B(A(err,[0]));};return ws;})"));}),_4w=function(_4x,_4y,_4z,_4A,_4B){return function(_){var _4C=nMV(_3U),_4D=_4C,_4E=[0,_4D],_4F=function(_4G){return [0,B(_3K(_4E,_4G,_3I))];};return [0,function(_){var _4H=B(A(_4v,[E(toJSStr(E(_4x))),function(_4I,_4J,_){return new F(function(){return _4e(new T(function(){return B(A(_4y,[_4I,_4J,_3I]));}),_C,_);});},function(_4K,_){return new F(function(){return _4e(new T(function(){return B(A(_4A,[_4K,_4F]));}),_C,_);});},function(_){return new F(function(){return _4e(new T(function(){return B(A(_4z,[_4F]));}),_C,_);});},_])),_4L=_4H;return new T(function(){return [0,B(_3V(_4E,_4B))];});}];};},_4M=function(_4N,_4O){var _4P=E(_4O);if(!_4P[0]){return [0,_C,_C];}else{var _4Q=_4P[1];if(!B(A(_4N,[_4Q]))){return [0,_C,_4P];}else{var _4R=new T(function(){var _4S=B(_4M(_4N,_4P[2]));return [0,_4S[1],_4S[2]];});return [0,[1,_4Q,new T(function(){return E(E(_4R)[1]);})],new T(function(){return E(E(_4R)[2]);})];}}},_4T=function(_4U){return new F(function(){return A(_4U,[_Y]);});},_4V=new T(function(){return B(unCStr("WebSockets connection died for some reason!"));}),_4W=new T(function(){return B(err(_4V));}),_4X=[1,_Y],_4Y=new T(function(){return B(unCStr("ServerException"));}),_4Z=new T(function(){return B(unCStr("Haste.App.Protocol"));}),_50=new T(function(){return B(unCStr("haste-lib-0.4.3"));}),_51=new T(function(){var _52=hs_wordToWord64(2080239195),_53=_52,_54=hs_wordToWord64(4130193019),_55=_54;return [0,_53,_55,[0,_53,_55,_50,_4Z,_4Y],_C];}),_56=function(_57){return E(_51);},_58=function(_59){return E(E(_59)[1]);},_5a=function(_5b,_5c,_5d){var _5e=B(A(_5b,[_])),_5f=B(A(_5c,[_])),_5g=hs_eqWord64(_5e[1],_5f[1]),_5h=_5g;if(!E(_5h)){return [0];}else{var _5i=hs_eqWord64(_5e[2],_5f[2]),_5j=_5i;return E(_5j)==0?[0]:[1,_5d];}},_5k=function(_5l){var _5m=E(_5l);return new F(function(){return _5a(B(_58(_5m[1])),_56,_5m[2]);});},_5n=[0,34],_5o=new T(function(){return B(unCStr("ServerException "));}),_5p=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_5q=new T(function(){return B(err(_5p));}),_5r=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_5s=new T(function(){return B(err(_5r));}),_5t=function(_5u,_5v){while(1){var _5w=E(_5u);if(!_5w[0]){return E(_5s);}else{var _5x=E(_5v);if(!_5x){return E(_5w[1]);}else{_5u=_5w[2];_5v=_5x-1|0;continue;}}}},_5y=new T(function(){return B(unCStr("ACK"));}),_5z=new T(function(){return B(unCStr("BEL"));}),_5A=new T(function(){return B(unCStr("BS"));}),_5B=new T(function(){return B(unCStr("SP"));}),_5C=[1,_5B,_C],_5D=new T(function(){return B(unCStr("US"));}),_5E=[1,_5D,_5C],_5F=new T(function(){return B(unCStr("RS"));}),_5G=[1,_5F,_5E],_5H=new T(function(){return B(unCStr("GS"));}),_5I=[1,_5H,_5G],_5J=new T(function(){return B(unCStr("FS"));}),_5K=[1,_5J,_5I],_5L=new T(function(){return B(unCStr("ESC"));}),_5M=[1,_5L,_5K],_5N=new T(function(){return B(unCStr("SUB"));}),_5O=[1,_5N,_5M],_5P=new T(function(){return B(unCStr("EM"));}),_5Q=[1,_5P,_5O],_5R=new T(function(){return B(unCStr("CAN"));}),_5S=[1,_5R,_5Q],_5T=new T(function(){return B(unCStr("ETB"));}),_5U=[1,_5T,_5S],_5V=new T(function(){return B(unCStr("SYN"));}),_5W=[1,_5V,_5U],_5X=new T(function(){return B(unCStr("NAK"));}),_5Y=[1,_5X,_5W],_5Z=new T(function(){return B(unCStr("DC4"));}),_60=[1,_5Z,_5Y],_61=new T(function(){return B(unCStr("DC3"));}),_62=[1,_61,_60],_63=new T(function(){return B(unCStr("DC2"));}),_64=[1,_63,_62],_65=new T(function(){return B(unCStr("DC1"));}),_66=[1,_65,_64],_67=new T(function(){return B(unCStr("DLE"));}),_68=[1,_67,_66],_69=new T(function(){return B(unCStr("SI"));}),_6a=[1,_69,_68],_6b=new T(function(){return B(unCStr("SO"));}),_6c=[1,_6b,_6a],_6d=new T(function(){return B(unCStr("CR"));}),_6e=[1,_6d,_6c],_6f=new T(function(){return B(unCStr("FF"));}),_6g=[1,_6f,_6e],_6h=new T(function(){return B(unCStr("VT"));}),_6i=[1,_6h,_6g],_6j=new T(function(){return B(unCStr("LF"));}),_6k=[1,_6j,_6i],_6l=new T(function(){return B(unCStr("HT"));}),_6m=[1,_6l,_6k],_6n=[1,_5A,_6m],_6o=[1,_5z,_6n],_6p=[1,_5y,_6o],_6q=new T(function(){return B(unCStr("ENQ"));}),_6r=[1,_6q,_6p],_6s=new T(function(){return B(unCStr("EOT"));}),_6t=[1,_6s,_6r],_6u=new T(function(){return B(unCStr("ETX"));}),_6v=[1,_6u,_6t],_6w=new T(function(){return B(unCStr("STX"));}),_6x=[1,_6w,_6v],_6y=new T(function(){return B(unCStr("SOH"));}),_6z=[1,_6y,_6x],_6A=new T(function(){return B(unCStr("NUL"));}),_6B=[1,_6A,_6z],_6C=[0,92],_6D=new T(function(){return B(unCStr("\\DEL"));}),_6E=new T(function(){return B(unCStr("\\a"));}),_6F=new T(function(){return B(unCStr("\\\\"));}),_6G=new T(function(){return B(unCStr("\\SO"));}),_6H=new T(function(){return B(unCStr("\\r"));}),_6I=new T(function(){return B(unCStr("\\f"));}),_6J=new T(function(){return B(unCStr("\\v"));}),_6K=new T(function(){return B(unCStr("\\n"));}),_6L=new T(function(){return B(unCStr("\\t"));}),_6M=new T(function(){return B(unCStr("\\b"));}),_6N=function(_6O,_6P){if(_6O<=127){var _6Q=E(_6O);switch(_6Q){case 92:return new F(function(){return _l(_6F,_6P);});break;case 127:return new F(function(){return _l(_6D,_6P);});break;default:if(_6Q<32){var _6R=E(_6Q);switch(_6R){case 7:return new F(function(){return _l(_6E,_6P);});break;case 8:return new F(function(){return _l(_6M,_6P);});break;case 9:return new F(function(){return _l(_6L,_6P);});break;case 10:return new F(function(){return _l(_6K,_6P);});break;case 11:return new F(function(){return _l(_6J,_6P);});break;case 12:return new F(function(){return _l(_6I,_6P);});break;case 13:return new F(function(){return _l(_6H,_6P);});break;case 14:return new F(function(){return _l(_6G,new T(function(){var _6S=E(_6P);if(!_6S[0]){var _6T=[0];}else{var _6T=E(E(_6S[1])[1])==72?B(unAppCStr("\\&",_6S)):E(_6S);}return _6T;}));});break;default:return new F(function(){return _l([1,_6C,new T(function(){var _6U=_6R;return _6U>=0?B(_5t(_6B,_6U)):E(_5q);})],_6P);});}}else{return [1,[0,_6Q],_6P];}}}else{return [1,_6C,new T(function(){var _6V=jsShowI(_6O),_6W=_6V;return B(_l(fromJSStr(_6W),new T(function(){var _6X=E(_6P);if(!_6X[0]){var _6Y=[0];}else{var _6Z=E(_6X[1])[1];if(_6Z<48){var _70=E(_6X);}else{var _70=_6Z>57?E(_6X):B(unAppCStr("\\&",_6X));}var _71=_70,_72=_71,_6Y=_72;}return _6Y;})));})];}},_73=new T(function(){return B(unCStr("\\\""));}),_74=function(_75,_76){var _77=E(_75);if(!_77[0]){return E(_76);}else{var _78=_77[2],_79=E(E(_77[1])[1]);if(_79==34){return new F(function(){return _l(_73,new T(function(){return B(_74(_78,_76));}));});}else{return new F(function(){return _6N(_79,new T(function(){return B(_74(_78,_76));}));});}}},_7a=function(_7b,_7c,_7d){return _7b<11?B(_l(_5o,[1,_5n,new T(function(){return B(_74(_7c,[1,_5n,_7d]));})])):[1,_v,new T(function(){return B(_l(_5o,[1,_5n,new T(function(){return B(_74(_7c,[1,_5n,[1,_u,_7d]]));})]));})];},_7e=function(_7f){return new F(function(){return _7a(0,E(_7f)[1],_C);});},_7g=function(_7h,_7i){return new F(function(){return _7a(0,E(_7h)[1],_7i);});},_7j=[0,44],_7k=[0,93],_7l=[0,91],_7m=function(_7n,_7o,_7p){var _7q=E(_7o);return _7q[0]==0?B(unAppCStr("[]",_7p)):[1,_7l,new T(function(){return B(A(_7n,[_7q[1],new T(function(){var _7r=function(_7s){var _7t=E(_7s);return _7t[0]==0?E([1,_7k,_7p]):[1,_7j,new T(function(){return B(A(_7n,[_7t[1],new T(function(){return B(_7r(_7t[2]));})]));})];};return B(_7r(_7q[2]));})]));})];},_7u=function(_7v,_7w){return new F(function(){return _7m(_7g,_7v,_7w);});},_7x=function(_7y,_7z,_7A){return new F(function(){return _7a(E(_7y)[1],E(_7z)[1],_7A);});},_7B=[0,_7x,_7e,_7u],_7C=new T(function(){return [0,_56,_7B,_7D,_5k];}),_7D=function(_7w){return [0,_7C,_7w];},_7E=function(_7F,_7G){return new F(function(){return die(new T(function(){return B(A(_7G,[_7F]));}));});},_7H=function(_7I){return new F(function(){return _7E(_7I,_7D);});},_7J=[0,0],_7K=function(_7L,_7M){return E(_7L)[1]!=E(_7M)[1];},_7N=function(_7O,_7P,_7Q){return [0,B(_3K(_7O,_7P,_7Q))];},_7R=new T(function(){return B(unCStr("Not enough data!"));}),_7S=[0,_7R],_7T=new T(function(){return B(_b("(function(s, msg) {s.send(msg);})"));}),_7U=function(_7V,_7W,_7X,_7Y,_){return [0,function(_){return new F(function(){return _4e([0,function(_){var _7Z=nMV(_C),_80=_7Z;return [0,function(_){var _81=nMV(_7J),_82=_81;return [0,function(_){var _83=nMV([0,function(_84,_85,_86){var _87=new T(function(){return E(E(_85)[1]);});return [0,B(_3V(_87,function(_88){return [0,B(_4w(new T(function(){return E(E(_7W)[1]);}),function(_89,_8a){return new F(function(){return (function(_8b){return function(_8c){return new F(function(){return A(new T(function(){return B(_4k(_8b));}),[function(_8d){return [0,function(_){var _8e=mMV(_80,function(_8f){if(!E(new T(function(){var _8g=E(_8d),_8h=B(A(E(_38)[1],[_8g[3],_8g[1]]));if(!_8h[0]){var _8i=E(_4X);}else{var _8j=E(_8h[1]),_8i=E(_8j[1])[1]>E(_8g[2])[1]?E(_4X):B(_7H(_8j[2]));}var _8k=_8i,_8l=_8k;return _8l;}))[0]){return [0,_8f,_4T];}else{var _8m=E(new T(function(){var _8n=E(_8d),_8o=B(A(E(_3E)[1],[_8n[3],_8n[1]]));if(!_8o[0]){var _8p=[0,_8o[1]];}else{var _8q=E(_8o[1]),_8p=E(_8q[1])[1]>E(_8n[2])[1]?E(_7S):[1,_8q[2]];}var _8r=_8p,_8s=_8r;return _8s;}));if(!_8m[0]){return [0,_8f,_4T];}else{var _8t=E(_8m[1]),_8u=B(_4M(function(_8v){return new F(function(){return _7K(E(_8v)[1],_8t[1]);});},_8f)),_8w=E(_8u[2]);return _8w[0]==0?[0,_8f,_4T]:[0,new T(function(){return B(_l(_8u[1],_8w[2]));}),function(_8x){return new F(function(){return _7N(E(_8w[1])[2],_8t[2],_8x);});}];}}}),_8y=_8e;return new T(function(){return B(A(_8y,[_8c]));});}];}]);});};})(_8a);});},_4W,_0,function(_8z){return [0,B(_3K(_87,function(_8A){return function(_8B,_8C){return [0,function(_){var _8D=B(A(new T(function(){return B(A(_7T,[E(_8z),E(_8A)]));}),[_])),_8E=_8D;return new T(function(){return B(A(_8C,[_Y]));});}];};},function(_8F){return E([0,function(_){var _8G=B(A(_7T,[E(_8z),E(_84),_])),_8H=_8G;return new T(function(){return B(A(_86,[_Y]));});}]);}))];}))];}))];},_C]),_8I=_83;return new T(function(){return B(A(_7V,[[0,[0,_8I],[0,_82],[0,_80]],_3I]));});}];}];}],_C,_);});},_7X,_7Y,_7W];},_8J=[0,1],_8K=[0,24601],_8L=new T(function(){return B(unCStr("ws://localhost:24601"));}),_8M=[0,_8L,_8K,_C],_8N=function(_8O,_){return _Y;},_8P=function(_8Q,_8R){while(1){var _8S=E(_8Q);if(!_8S[0]){return E(_8R);}else{_8Q=_8S[2];var _8T=_8R+1|0;_8R=_8T;continue;}}},_8U=new T(function(){return [0,"Uint32Array"];}),_8V=function(_8W){return function(_8X,_){var _8Y=B(A(_b,[E(_Z)[1],E(_8X),E(new T(function(){return B(_7(function(_){var _=0;return new F(function(){return A(_1c,[_15,_8U,_16,new T(function(){return E(E(_8W)[1]);}),_]);});}));})),_])),_8Z=_8Y;return _Y;};},_90=function(_91){return E(E(_91)[2]);},_92=function(_93,_94){return function(_95,_){var _96=B(A(B(_8V(new T(function(){return [0,B(_8P(_94,0))>>>0];}))),[_95,_])),_97=_96;return new F(function(){return A(E(new T(function(){var _98=function(_99){var _9a=E(_99);return _9a[0]==0?_8N:function(_9b,_){var _9c=B(A(B(A(new T(function(){return B(_90(_93));}),[_9a[1]]))[1],[_9b,_])),_9d=_9c;return new F(function(){return A(E(new T(function(){return [0,B(_98(_9a[2]))];}))[1],[_9b,_]);});};};return [0,B(_98(_94))];}))[1],[_95,_]);});};},_9e=function(_9f){return [0,function(_9g,_){var _9h=B(A(B(_1h(new T(function(){return [0,B(_7(function(_){var _=0,_9i=B(A(_3g,[E(_9f),_])),_9j=_9i;return new T(function(){return B(_3c(_9j));});}))[1]];}))),[_9g,_])),_9k=_9h,_9l=B(A(_b,[E(_Z)[1],E(_9g),E(_9f),_])),_9m=_9l;return _Y;}];},_9n=[0,_3v,_9e],_9o=[0,0],_9p=[0,1],_9q=new T(function(){return [0,"Uint8Array"];}),_9r=function(_9s){return function(_9t,_){var _9u=B(A(_b,[E(_Z)[1],E(_9t),E(new T(function(){return B(_7(function(_){var _=0;return new F(function(){return A(_1c,[_15,_9q,_9p,new T(function(){return E(E(_9s)[1]);}),_]);});}));})),_])),_9v=_9u;return _Y;};},_9w=new T(function(){return [0,B(_9r(_9o))];}),_9x=function(_9y,_9z,_9A){return function(_9B,_){var _9C=B(A(E(_9w)[1],[_9B,_])),_9D=_9C,_9E=B(A(E(new T(function(){return [0,B(_1h(new T(function(){return [0,E(_9y)[1]];})))];}))[1],[_9B,_])),_9F=_9E,_9G=B(A(E(new T(function(){return [0,B(_1h(new T(function(){return [0,E(_9z)[1]];})))];}))[1],[_9B,_])),_9H=_9G;return new F(function(){return A(E(new T(function(){return [0,B(_92(_9n,_9A))];}))[1],[_9B,_]);});};},_9I=new T(function(){return B(unCStr("(function(){return [];})"));}),_9J=new T(function(){return B(_b("(function(parts){return new Blob(parts);})"));}),_9K=function(_9L){return new F(function(){return _7(function(_){var _=0,_9M=B(A(_b,[toJSStr(E(_9I)),_])),_9N=_9M,_9O=B(A(_9L,[_9N,_])),_9P=_9O;return new F(function(){return A(_9J,[E(_9N),_]);});});});},_9Q=function(_9R){return [0,new T(function(){return [0,E(_9R)[1]+1|0];}),_9R];},_9S=new T(function(){return B(unCStr("Unable to decode return value!"));}),_9T=new T(function(){return B(err(_9S));}),_9U=function(_9V,_9W){while(1){var _9X=E(_9V);if(!_9X[0]){return E(_9W);}else{_9V=_9X[2];var _9Y=[1,_9X[1],_9W];_9W=_9Y;continue;}}},_9Z=function(_a0,_a1,_a2){return function(_a3,_a4){var _a5=new T(function(){return E(E(_a3)[1]);});return [0,B(_3V(_a5,function(_a6){return [0,B(_3K(_a5,_a6,function(_a7){return E([0,function(_){var _a8=nMV(_3U),_a9=_a8,_aa=[0,_a9];return [0,function(_){var _ab=E(_a3),_ac=mMV(E(_ab[2])[1],_9Q),_ad=_ac;return [0,function(_){var _ae=mMV(E(_ab[3])[1],function(_af){return [0,[1,[0,_ad,_aa],_af],_Y];}),_ag=_ae;return new T(function(){return B(A(_a6,[new T(function(){return B(_9K(B(_9x(_ad,_a1,new T(function(){return B(_9U(_a2,_C));})))));}),_ab,function(_ah){return [0,B(_3V(_aa,function(_ai){return new F(function(){return A(_4k,[_ai,function(_aj){var _ak=E(_aj),_al=B(A(E(new T(function(){return B(_2v(_a0));}))[1],[_ak[3],_ak[1]]));if(!_al[0]){return E(_9T);}else{var _am=E(_al[1]);return E(_am[1])[1]>E(_ak[2])[1]?E(_9T):B(A(_a4,[_am[2]]));}}]);});}))];}]));});}];}];}]);}))];}))];};},_an=new T(function(){return [0,"click"];}),_ao=function(_ap){return [0,function(_){var _aq=jsAlert(toJSStr(B(unAppCStr("The old message was: ",_ap))));return _3H;}];},_ar=function(_as){return [0,B(_2H(_as))];},_at=function(_au,_av){return [0,B(_92(_au,_av))];},_aw=function(_ax){return [0,new T(function(){return B(_ar(_ax));}),function(_ay){return new F(function(){return _at(_ax,_ay);});}];},_az=new T(function(){return B(_aw(_1o));}),_aA=new T(function(){return B(unCStr("Enter a message"));}),_aB=new T(function(){return B(unCStr("button"));}),_aC=[0,0],_aD=new T(function(){return B(unCStr(" could be found!"));}),_aE=function(_aF){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_l(_aF,_aD));}))));});},_aG=function(_aH,_aI){return [0,function(_){var _aJ=E(_aB),_aK=jsFind(toJSStr(_aJ)),_aL=_aK;return new T(function(){var _aM=E(_aL);if(!_aM[0]){var _aN=B(_aE(_aJ));}else{var _aN=[0,function(_){var _aO=jsSetCB(E(_aM[1])[1],E(_an)[1],function(_aP,_aQ,_){return new F(function(){return _4e([0,function(_){var _aR=jsPrompt(toJSStr(E(_aA))),_aS=_aR;return new T(function(){return B(A(_9Z,[_az,_aC,[1,new T(function(){return B(_9K(B(_92(_1o,new T(function(){return fromJSStr(_aS);})))));}),_C],_aH,_ao]));});}],_C,_);});}),_aT=_aO;return new T(function(){return B(A(_aI,[_Y]));});}];}return _aN;});}];},_aU=new T(function(){return B(unCStr("Prelude.undefined"));}),_aV=new T(function(){return B(err(_aU));}),_aW=function(_){var _aX=B(_7U(_aG,_8M,_8J,_aV,_)),_aY=_aX;return new F(function(){return A(E(_aY)[1],[_]);});},_aZ=function(_){return new F(function(){return _aW(_);});};
var hasteMain = function() {B(A(_aZ, [0]));};window.onload = hasteMain;