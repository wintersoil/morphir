(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.fT.aJ === region.eq.aJ)
	{
		return 'on line ' + region.fT.aJ;
	}
	return 'on lines ' + region.fT.aJ + ' through ' + region.eq.aJ;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eP,
		impl.gp,
		impl.f$,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.e8) { flags += 'm'; }
	if (options.d5) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = 2;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.r) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.u),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.u);
		} else {
			var treeLen = builder.r * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.v) : builder.v;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.r);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.u) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.u);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{v: nodeList, r: (len / $elm$core$Array$branchFactor) | 0, u: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Morphir$Elm$CLI$Generate = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Elm$CLI$PackageDefinitionFromSource = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Morphir$Elm$CLI$generate = _Platform_incomingPort(
	'generate',
	A2(
		$elm$json$Json$Decode$andThen,
		function (_v0) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (_v1) {
					return $elm$json$Json$Decode$succeed(
						_Utils_Tuple2(_v0, _v1));
				},
				A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$value));
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$value)));
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Morphir$Elm$CLI$packageDefinitionFromSource = _Platform_incomingPort(
	'packageDefinitionFromSource',
	A2(
		$elm$json$Json$Decode$andThen,
		function (_v0) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (_v1) {
					return $elm$json$Json$Decode$succeed(
						_Utils_Tuple2(_v0, _v1));
				},
				A2(
					$elm$json$Json$Decode$index,
					1,
					$elm$json$Json$Decode$list(
						A2(
							$elm$json$Json$Decode$andThen,
							function (path) {
								return A2(
									$elm$json$Json$Decode$andThen,
									function (content) {
										return $elm$json$Json$Decode$succeed(
											{ee: content, fw: path});
									},
									A2($elm$json$Json$Decode$field, 'content', $elm$json$Json$Decode$string));
							},
							A2($elm$json$Json$Decode$field, 'path', $elm$json$Json$Decode$string)))));
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$value)));
var $author$project$Morphir$Elm$CLI$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Morphir$Elm$CLI$packageDefinitionFromSource($author$project$Morphir$Elm$CLI$PackageDefinitionFromSource),
				$author$project$Morphir$Elm$CLI$generate($author$project$Morphir$Elm$CLI$Generate)
			]));
};
var $author$project$Morphir$IR$Distribution$Library = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $author$project$Morphir$IR$Package$Definition = function (modules) {
	return {e7: modules};
};
var $author$project$Morphir$IR$AccessControlled$AccessControlled = F2(
	function (access, value) {
		return {b0: access, aT: value};
	});
var $author$project$Morphir$IR$AccessControlled$Private = 1;
var $author$project$Morphir$IR$AccessControlled$Public = 0;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$map = _Json_map1;
var $author$project$Morphir$IR$AccessControlled$Codec$decodeAccessControlled = function (decodeValue) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (tag) {
			switch (tag) {
				case 'public':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Morphir$IR$AccessControlled$AccessControlled(0),
						A2($elm$json$Json$Decode$index, 1, decodeValue));
				case 'private':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Morphir$IR$AccessControlled$AccessControlled(1),
						A2($elm$json$Json$Decode$index, 1, decodeValue));
				default:
					var other = tag;
					return $elm$json$Json$Decode$fail('Unknown access controlled type: ' + other);
			}
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
};
var $author$project$Morphir$IR$Module$Definition = F2(
	function (types, values) {
		return {gn: types, gq: values};
	});
var $author$project$Morphir$IR$Type$CustomTypeDefinition = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Morphir$IR$Type$TypeAliasDefinition = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$IR$Type$Constructor = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$IR$Name$fromList = function (words) {
	return words;
};
var $author$project$Morphir$IR$Name$Codec$decodeName = A2(
	$elm$json$Json$Decode$map,
	$author$project$Morphir$IR$Name$fromList,
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string));
var $author$project$Morphir$IR$Type$ExtensibleRecord = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Type$Field = F2(
	function (name, tpe) {
		return {cN: name, gl: tpe};
	});
var $author$project$Morphir$IR$Type$Function = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Type$Record = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Morphir$IR$Type$Reference = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Type$Tuple = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$IR$Type$Unit = function (a) {
	return {$: 6, a: a};
};
var $author$project$Morphir$IR$Type$Variable = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$IR$FQName$FQName = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Path$fromList = function (names) {
	return names;
};
var $author$project$Morphir$IR$Path$Codec$decodePath = A2(
	$elm$json$Json$Decode$map,
	$author$project$Morphir$IR$Path$fromList,
	$elm$json$Json$Decode$list($author$project$Morphir$IR$Name$Codec$decodeName));
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$Morphir$IR$FQName$Codec$decodeFQName = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Morphir$IR$FQName$FQName,
	A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Path$Codec$decodePath),
	A2($elm$json$Json$Decode$index, 1, $author$project$Morphir$IR$Path$Codec$decodePath),
	A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Name$Codec$decodeName));
var $elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		$elm$json$Json$Decode$andThen,
		thunk,
		$elm$json$Json$Decode$succeed(0));
};
var $elm$json$Json$Decode$map2 = _Json_map2;
var $author$project$Morphir$IR$Type$Codec$decodeField = function (decodeAttributes) {
	return A3(
		$elm$json$Json$Decode$map2,
		$author$project$Morphir$IR$Type$Field,
		A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
		A2(
			$elm$json$Json$Decode$index,
			1,
			$author$project$Morphir$IR$Type$Codec$decodeType(decodeAttributes)));
};
var $author$project$Morphir$IR$Type$Codec$decodeType = function (decodeAttributes) {
	var lazyDecodeType = $elm$json$Json$Decode$lazy(
		function (_v3) {
			return $author$project$Morphir$IR$Type$Codec$decodeType(decodeAttributes);
		});
	var lazyDecodeField = $elm$json$Json$Decode$lazy(
		function (_v2) {
			return $author$project$Morphir$IR$Type$Codec$decodeField(decodeAttributes);
		});
	return A2(
		$elm$json$Json$Decode$andThen,
		function (kind) {
			switch (kind) {
				case 'variable':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$Variable,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Name$Codec$decodeName));
				case 'reference':
					return A4(
						$elm$json$Json$Decode$map3,
						$author$project$Morphir$IR$Type$Reference,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$FQName$Codec$decodeFQName),
						A2(
							$elm$json$Json$Decode$index,
							3,
							$elm$json$Json$Decode$list(
								$elm$json$Json$Decode$lazy(
									function (_v1) {
										return $author$project$Morphir$IR$Type$Codec$decodeType(decodeAttributes);
									}))));
				case 'tuple':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$Tuple,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$elm$json$Json$Decode$list(lazyDecodeType)));
				case 'record':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$Record,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$elm$json$Json$Decode$list(lazyDecodeField)));
				case 'extensible_record':
					return A4(
						$elm$json$Json$Decode$map3,
						$author$project$Morphir$IR$Type$ExtensibleRecord,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Name$Codec$decodeName),
						A2(
							$elm$json$Json$Decode$index,
							3,
							$elm$json$Json$Decode$list(lazyDecodeField)));
				case 'function':
					return A4(
						$elm$json$Json$Decode$map3,
						$author$project$Morphir$IR$Type$Function,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, lazyDecodeType),
						A2($elm$json$Json$Decode$index, 3, lazyDecodeType));
				case 'unit':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Morphir$IR$Type$Unit,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes));
				default:
					return $elm$json$Json$Decode$fail('Unknown kind: ' + kind);
			}
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
};
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Morphir$IR$Type$Codec$decodeConstructors = function (decodeAttributes) {
	return $elm$json$Json$Decode$list(
		A2(
			$elm$json$Json$Decode$andThen,
			function (kind) {
				if (kind === 'constructor') {
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$Constructor,
						A2($elm$json$Json$Decode$index, 1, $author$project$Morphir$IR$Name$Codec$decodeName),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$elm$json$Json$Decode$list(
								A3(
									$elm$json$Json$Decode$map2,
									$elm$core$Tuple$pair,
									A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
									A2(
										$elm$json$Json$Decode$index,
										1,
										$author$project$Morphir$IR$Type$Codec$decodeType(decodeAttributes))))));
				} else {
					return $elm$json$Json$Decode$fail('Unknown kind: ' + kind);
				}
			},
			A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string)));
};
var $author$project$Morphir$IR$Type$Codec$decodeDefinition = function (decodeAttributes) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (kind) {
			switch (kind) {
				case 'type_alias_definition':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$TypeAliasDefinition,
						A2(
							$elm$json$Json$Decode$index,
							1,
							$elm$json$Json$Decode$list($author$project$Morphir$IR$Name$Codec$decodeName)),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$author$project$Morphir$IR$Type$Codec$decodeType(decodeAttributes)));
				case 'custom_type_definition':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$CustomTypeDefinition,
						A2(
							$elm$json$Json$Decode$index,
							1,
							$elm$json$Json$Decode$list($author$project$Morphir$IR$Name$Codec$decodeName)),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$author$project$Morphir$IR$AccessControlled$Codec$decodeAccessControlled(
								$author$project$Morphir$IR$Type$Codec$decodeConstructors(decodeAttributes))));
				default:
					return $elm$json$Json$Decode$fail('Unknown kind: ' + kind);
			}
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
};
var $author$project$Morphir$IR$Value$Apply = F3(
	function (a, b, c) {
		return {$: 9, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$Constructor = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$Definition = F3(
	function (inputTypes, outputType, body) {
		return {dV: body, a3: inputTypes, fq: outputType};
	});
var $author$project$Morphir$IR$Value$Destructure = F4(
	function (a, b, c, d) {
		return {$: 13, a: a, b: b, c: c, d: d};
	});
var $author$project$Morphir$IR$Value$Field = F3(
	function (a, b, c) {
		return {$: 7, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$FieldFunction = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$IfThenElse = F4(
	function (a, b, c, d) {
		return {$: 14, a: a, b: b, c: c, d: d};
	});
var $author$project$Morphir$IR$Value$Lambda = F3(
	function (a, b, c) {
		return {$: 10, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$LetDefinition = F4(
	function (a, b, c, d) {
		return {$: 11, a: a, b: b, c: c, d: d};
	});
var $author$project$Morphir$IR$Value$LetRecursion = F3(
	function (a, b, c) {
		return {$: 12, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$List = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$Literal = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$PatternMatch = F3(
	function (a, b, c) {
		return {$: 15, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$Record = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$Reference = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$Tuple = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$Unit = function (a) {
	return {$: 17, a: a};
};
var $author$project$Morphir$IR$Value$UpdateRecord = F3(
	function (a, b, c) {
		return {$: 16, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$Variable = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Morphir$IR$Literal$BoolLiteral = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$IR$Literal$CharLiteral = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$IR$Literal$FloatLiteral = function (a) {
	return {$: 4, a: a};
};
var $author$project$Morphir$IR$Literal$IntLiteral = function (a) {
	return {$: 3, a: a};
};
var $author$project$Morphir$IR$Literal$StringLiteral = function (a) {
	return {$: 2, a: a};
};
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Morphir$IR$Literal$Codec$decodeLiteral = A2(
	$elm$json$Json$Decode$andThen,
	function (kind) {
		switch (kind) {
			case 'bool_literal':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Morphir$IR$Literal$BoolLiteral,
					A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$bool));
			case 'char_literal':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Morphir$IR$Literal$CharLiteral,
					A2(
						$elm$json$Json$Decode$andThen,
						function (str) {
							var _v1 = $elm$core$String$uncons(str);
							if (!_v1.$) {
								var _v2 = _v1.a;
								var ch = _v2.a;
								return $elm$json$Json$Decode$succeed(ch);
							} else {
								return $elm$json$Json$Decode$fail('Single char expected');
							}
						},
						A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string)));
			case 'string_literal':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Morphir$IR$Literal$StringLiteral,
					A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string));
			case 'int_literal':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Morphir$IR$Literal$IntLiteral,
					A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$int));
			case 'float_literal':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Morphir$IR$Literal$FloatLiteral,
					A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
			default:
				var other = kind;
				return $elm$json$Json$Decode$fail('Unknown literal type: ' + other);
		}
	},
	A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
var $author$project$Morphir$IR$Value$AsPattern = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$ConstructorPattern = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$EmptyListPattern = function (a) {
	return {$: 4, a: a};
};
var $author$project$Morphir$IR$Value$HeadTailPattern = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $author$project$Morphir$IR$Value$LiteralPattern = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$TuplePattern = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$IR$Value$UnitPattern = function (a) {
	return {$: 7, a: a};
};
var $author$project$Morphir$IR$Value$WildcardPattern = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$IR$Value$Codec$decodePattern = function (decodeAttributes) {
	var lazyDecodePattern = $elm$json$Json$Decode$lazy(
		function (_v1) {
			return $author$project$Morphir$IR$Value$Codec$decodePattern(decodeAttributes);
		});
	return A2(
		$elm$json$Json$Decode$andThen,
		function (kind) {
			switch (kind) {
				case 'wildcard_pattern':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Morphir$IR$Value$WildcardPattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes));
				case 'as_pattern':
					return A4(
						$elm$json$Json$Decode$map3,
						$author$project$Morphir$IR$Value$AsPattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, lazyDecodePattern),
						A2($elm$json$Json$Decode$index, 3, $author$project$Morphir$IR$Name$Codec$decodeName));
				case 'tuple_pattern':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Value$TuplePattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$elm$json$Json$Decode$list(lazyDecodePattern)));
				case 'constructor_pattern':
					return A4(
						$elm$json$Json$Decode$map3,
						$author$project$Morphir$IR$Value$ConstructorPattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$FQName$Codec$decodeFQName),
						A2(
							$elm$json$Json$Decode$index,
							3,
							$elm$json$Json$Decode$list(lazyDecodePattern)));
				case 'empty_list_pattern':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Morphir$IR$Value$EmptyListPattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes));
				case 'head_tail_pattern':
					return A4(
						$elm$json$Json$Decode$map3,
						$author$project$Morphir$IR$Value$HeadTailPattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, lazyDecodePattern),
						A2($elm$json$Json$Decode$index, 3, lazyDecodePattern));
				case 'literal_pattern':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Value$LiteralPattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes),
						A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Literal$Codec$decodeLiteral));
				case 'unit_pattern':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Morphir$IR$Value$UnitPattern,
						A2($elm$json$Json$Decode$index, 1, decodeAttributes));
				default:
					var other = kind;
					return $elm$json$Json$Decode$fail('Unknown pattern type: ' + other);
			}
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Morphir$IR$Value$Codec$decodeDefinition = F2(
	function (decodeTypeAttributes, decodeValueAttributes) {
		return A4(
			$elm$json$Json$Decode$map3,
			$author$project$Morphir$IR$Value$Definition,
			A2(
				$elm$json$Json$Decode$field,
				'inputTypes',
				$elm$json$Json$Decode$list(
					A4(
						$elm$json$Json$Decode$map3,
						F3(
							function (n, a, t) {
								return _Utils_Tuple3(n, a, t);
							}),
						A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
						A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$author$project$Morphir$IR$Type$Codec$decodeType(decodeTypeAttributes))))),
			A2(
				$elm$json$Json$Decode$field,
				'outputType',
				$author$project$Morphir$IR$Type$Codec$decodeType(decodeTypeAttributes)),
			A2(
				$elm$json$Json$Decode$field,
				'body',
				$elm$json$Json$Decode$lazy(
					function (_v2) {
						return A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes);
					})));
	});
var $author$project$Morphir$IR$Value$Codec$decodeValue = F2(
	function (decodeTypeAttributes, decodeValueAttributes) {
		var lazyDecodeValue = $elm$json$Json$Decode$lazy(
			function (_v1) {
				return A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes);
			});
		return A2(
			$elm$json$Json$Decode$andThen,
			function (kind) {
				switch (kind) {
					case 'literal':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$Literal,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Literal$Codec$decodeLiteral));
					case 'constructor':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$Constructor,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$FQName$Codec$decodeFQName));
					case 'tuple':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$Tuple,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								$elm$json$Json$Decode$list(lazyDecodeValue)));
					case 'list':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$List,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								$elm$json$Json$Decode$list(lazyDecodeValue)));
					case 'record':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$Record,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								$elm$json$Json$Decode$list(
									A3(
										$elm$json$Json$Decode$map2,
										$elm$core$Tuple$pair,
										A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
										A2(
											$elm$json$Json$Decode$index,
											1,
											A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes))))));
					case 'variable':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$Variable,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Name$Codec$decodeName));
					case 'reference':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$Reference,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$FQName$Codec$decodeFQName));
					case 'field':
						return A4(
							$elm$json$Json$Decode$map3,
							$author$project$Morphir$IR$Value$Field,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)),
							A2($elm$json$Json$Decode$index, 3, $author$project$Morphir$IR$Name$Codec$decodeName));
					case 'field_function':
						return A3(
							$elm$json$Json$Decode$map2,
							$author$project$Morphir$IR$Value$FieldFunction,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Name$Codec$decodeName));
					case 'apply':
						return A4(
							$elm$json$Json$Decode$map3,
							$author$project$Morphir$IR$Value$Apply,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								3,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)));
					case 'lambda':
						return A4(
							$elm$json$Json$Decode$map3,
							$author$project$Morphir$IR$Value$Lambda,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								$author$project$Morphir$IR$Value$Codec$decodePattern(decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								3,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)));
					case 'let_definition':
						return A5(
							$elm$json$Json$Decode$map4,
							$author$project$Morphir$IR$Value$LetDefinition,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2($elm$json$Json$Decode$index, 2, $author$project$Morphir$IR$Name$Codec$decodeName),
							A2(
								$elm$json$Json$Decode$index,
								3,
								A2($author$project$Morphir$IR$Value$Codec$decodeDefinition, decodeTypeAttributes, decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								4,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)));
					case 'let_recursion':
						return A4(
							$elm$json$Json$Decode$map3,
							$author$project$Morphir$IR$Value$LetRecursion,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								A2(
									$elm$json$Json$Decode$map,
									$elm$core$Dict$fromList,
									$elm$json$Json$Decode$list(
										A3(
											$elm$json$Json$Decode$map2,
											$elm$core$Tuple$pair,
											A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
											A2(
												$elm$json$Json$Decode$index,
												1,
												A2($author$project$Morphir$IR$Value$Codec$decodeDefinition, decodeTypeAttributes, decodeValueAttributes)))))),
							A2(
								$elm$json$Json$Decode$index,
								3,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)));
					case 'destructure':
						return A5(
							$elm$json$Json$Decode$map4,
							$author$project$Morphir$IR$Value$Destructure,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								$author$project$Morphir$IR$Value$Codec$decodePattern(decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								3,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								4,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)));
					case 'if_then_else':
						return A5(
							$elm$json$Json$Decode$map4,
							$author$project$Morphir$IR$Value$IfThenElse,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								3,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								4,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)));
					case 'pattern_match':
						return A4(
							$elm$json$Json$Decode$map3,
							$author$project$Morphir$IR$Value$PatternMatch,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								3,
								$elm$json$Json$Decode$list(
									A3(
										$elm$json$Json$Decode$map2,
										$elm$core$Tuple$pair,
										A2(
											$elm$json$Json$Decode$index,
											0,
											$author$project$Morphir$IR$Value$Codec$decodePattern(decodeValueAttributes)),
										A2(
											$elm$json$Json$Decode$index,
											1,
											A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes))))));
					case 'update_record':
						return A4(
							$elm$json$Json$Decode$map3,
							$author$project$Morphir$IR$Value$UpdateRecord,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes),
							A2(
								$elm$json$Json$Decode$index,
								2,
								A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes)),
							A2(
								$elm$json$Json$Decode$index,
								3,
								$elm$json$Json$Decode$list(
									A3(
										$elm$json$Json$Decode$map2,
										$elm$core$Tuple$pair,
										A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
										A2(
											$elm$json$Json$Decode$index,
											1,
											A2($author$project$Morphir$IR$Value$Codec$decodeValue, decodeTypeAttributes, decodeValueAttributes))))));
					case 'unit':
						return A2(
							$elm$json$Json$Decode$map,
							$author$project$Morphir$IR$Value$Unit,
							A2($elm$json$Json$Decode$index, 1, decodeValueAttributes));
					default:
						var other = kind;
						return $elm$json$Json$Decode$fail('Unknown value type: ' + other);
				}
			},
			A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
	});
var $author$project$Morphir$IR$Documented$Documented = F2(
	function (doc, value) {
		return {en: doc, aT: value};
	});
var $author$project$Morphir$IR$Documented$Codec$decodeDocumented = function (decodeValue) {
	return A3(
		$elm$json$Json$Decode$map2,
		$author$project$Morphir$IR$Documented$Documented,
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$index, 1, decodeValue));
};
var $author$project$Morphir$IR$Module$Codec$decodeDefinition = F2(
	function (decodeTypeAttributes, decodeValueAttributes) {
		return A3(
			$elm$json$Json$Decode$map2,
			$author$project$Morphir$IR$Module$Definition,
			A2(
				$elm$json$Json$Decode$field,
				'types',
				A2(
					$elm$json$Json$Decode$map,
					$elm$core$Dict$fromList,
					$elm$json$Json$Decode$list(
						A3(
							$elm$json$Json$Decode$map2,
							$elm$core$Tuple$pair,
							A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
							A2(
								$elm$json$Json$Decode$index,
								1,
								$author$project$Morphir$IR$AccessControlled$Codec$decodeAccessControlled(
									$author$project$Morphir$IR$Documented$Codec$decodeDocumented(
										$author$project$Morphir$IR$Type$Codec$decodeDefinition(decodeTypeAttributes)))))))),
			A2(
				$elm$json$Json$Decode$field,
				'values',
				A2(
					$elm$json$Json$Decode$map,
					$elm$core$Dict$fromList,
					$elm$json$Json$Decode$list(
						A3(
							$elm$json$Json$Decode$map2,
							$elm$core$Tuple$pair,
							A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
							A2(
								$elm$json$Json$Decode$index,
								1,
								$author$project$Morphir$IR$AccessControlled$Codec$decodeAccessControlled(
									A2($author$project$Morphir$IR$Value$Codec$decodeDefinition, decodeTypeAttributes, decodeValueAttributes))))))));
	});
var $author$project$Morphir$IR$Package$Codec$decodeDefinition = F2(
	function (decodeAttributes, decodeAttributes2) {
		return A2(
			$elm$json$Json$Decode$map,
			$author$project$Morphir$IR$Package$Definition,
			A2(
				$elm$json$Json$Decode$field,
				'modules',
				A2(
					$elm$json$Json$Decode$map,
					$elm$core$Dict$fromList,
					$elm$json$Json$Decode$list(
						A3(
							$elm$json$Json$Decode$map2,
							$elm$core$Tuple$pair,
							A2($elm$json$Json$Decode$field, 'name', $author$project$Morphir$IR$Path$Codec$decodePath),
							A2(
								$elm$json$Json$Decode$field,
								'def',
								$author$project$Morphir$IR$AccessControlled$Codec$decodeAccessControlled(
									A2($author$project$Morphir$IR$Module$Codec$decodeDefinition, decodeAttributes, decodeAttributes2))))))));
	});
var $author$project$Morphir$IR$Package$Specification = function (modules) {
	return {e7: modules};
};
var $author$project$Morphir$IR$Module$Specification = F2(
	function (types, values) {
		return {gn: types, gq: values};
	});
var $author$project$Morphir$IR$Type$CustomTypeSpecification = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$IR$Type$OpaqueTypeSpecification = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$IR$Type$TypeAliasSpecification = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$IR$Type$Codec$decodeSpecification = function (decodeAttributes) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (kind) {
			switch (kind) {
				case 'type_alias_specification':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$TypeAliasSpecification,
						A2(
							$elm$json$Json$Decode$index,
							1,
							$elm$json$Json$Decode$list($author$project$Morphir$IR$Name$Codec$decodeName)),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$author$project$Morphir$IR$Type$Codec$decodeType(decodeAttributes)));
				case 'opaque_type_specification':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Morphir$IR$Type$OpaqueTypeSpecification,
						A2(
							$elm$json$Json$Decode$index,
							1,
							$elm$json$Json$Decode$list($author$project$Morphir$IR$Name$Codec$decodeName)));
				case 'custom_type_specification':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Morphir$IR$Type$CustomTypeSpecification,
						A2(
							$elm$json$Json$Decode$index,
							1,
							$elm$json$Json$Decode$list($author$project$Morphir$IR$Name$Codec$decodeName)),
						A2(
							$elm$json$Json$Decode$index,
							2,
							$author$project$Morphir$IR$Type$Codec$decodeConstructors(decodeAttributes)));
				default:
					return $elm$json$Json$Decode$fail('Unknown kind: ' + kind);
			}
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
};
var $author$project$Morphir$IR$Value$Specification = F2(
	function (inputs, output) {
		return {bo: inputs, bN: output};
	});
var $author$project$Morphir$IR$Value$Codec$decodeSpecification = function (decodeTypeAttributes) {
	return A3(
		$elm$json$Json$Decode$map2,
		$author$project$Morphir$IR$Value$Specification,
		A2(
			$elm$json$Json$Decode$field,
			'inputs',
			$elm$json$Json$Decode$list(
				A3(
					$elm$json$Json$Decode$map2,
					$elm$core$Tuple$pair,
					A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
					A2(
						$elm$json$Json$Decode$index,
						1,
						$author$project$Morphir$IR$Type$Codec$decodeType(decodeTypeAttributes))))),
		A2(
			$elm$json$Json$Decode$field,
			'output',
			$author$project$Morphir$IR$Type$Codec$decodeType(decodeTypeAttributes)));
};
var $author$project$Morphir$IR$Module$Codec$decodeSpecification = function (decodeTypeAttributes) {
	return A3(
		$elm$json$Json$Decode$map2,
		$author$project$Morphir$IR$Module$Specification,
		A2(
			$elm$json$Json$Decode$field,
			'types',
			A2(
				$elm$json$Json$Decode$map,
				$elm$core$Dict$fromList,
				$elm$json$Json$Decode$list(
					A3(
						$elm$json$Json$Decode$map2,
						$elm$core$Tuple$pair,
						A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
						A2(
							$elm$json$Json$Decode$index,
							1,
							$author$project$Morphir$IR$Documented$Codec$decodeDocumented(
								$author$project$Morphir$IR$Type$Codec$decodeSpecification(decodeTypeAttributes))))))),
		A2(
			$elm$json$Json$Decode$field,
			'values',
			A2(
				$elm$json$Json$Decode$map,
				$elm$core$Dict$fromList,
				$elm$json$Json$Decode$list(
					A3(
						$elm$json$Json$Decode$map2,
						$elm$core$Tuple$pair,
						A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Name$Codec$decodeName),
						A2(
							$elm$json$Json$Decode$index,
							1,
							$author$project$Morphir$IR$Value$Codec$decodeSpecification(decodeTypeAttributes)))))));
};
var $author$project$Morphir$IR$Package$Codec$decodeSpecification = function (decodeAttributes) {
	return A2(
		$elm$json$Json$Decode$map,
		$author$project$Morphir$IR$Package$Specification,
		A2(
			$elm$json$Json$Decode$field,
			'modules',
			A2(
				$elm$json$Json$Decode$map,
				$elm$core$Dict$fromList,
				$elm$json$Json$Decode$list(
					A3(
						$elm$json$Json$Decode$map2,
						$elm$core$Tuple$pair,
						A2($elm$json$Json$Decode$field, 'name', $author$project$Morphir$IR$Path$Codec$decodePath),
						A2(
							$elm$json$Json$Decode$field,
							'spec',
							$author$project$Morphir$IR$Module$Codec$decodeSpecification(decodeAttributes)))))));
};
var $author$project$Morphir$Codec$decodeUnit = $elm$json$Json$Decode$succeed(0);
var $author$project$Morphir$IR$Distribution$Codec$decodeDistribution = A2(
	$elm$json$Json$Decode$andThen,
	function (kind) {
		if (kind === 'library') {
			return A4(
				$elm$json$Json$Decode$map3,
				$author$project$Morphir$IR$Distribution$Library,
				A2($elm$json$Json$Decode$index, 1, $author$project$Morphir$IR$Path$Codec$decodePath),
				A2(
					$elm$json$Json$Decode$index,
					2,
					A2(
						$elm$json$Json$Decode$map,
						$elm$core$Dict$fromList,
						$elm$json$Json$Decode$list(
							A3(
								$elm$json$Json$Decode$map2,
								$elm$core$Tuple$pair,
								A2($elm$json$Json$Decode$index, 0, $author$project$Morphir$IR$Path$Codec$decodePath),
								A2(
									$elm$json$Json$Decode$index,
									1,
									$author$project$Morphir$IR$Package$Codec$decodeSpecification($author$project$Morphir$Codec$decodeUnit)))))),
				A2(
					$elm$json$Json$Decode$index,
					3,
					A2(
						$author$project$Morphir$IR$Package$Codec$decodeDefinition,
						$author$project$Morphir$Codec$decodeUnit,
						$author$project$Morphir$IR$Type$Codec$decodeType($author$project$Morphir$Codec$decodeUnit))));
		} else {
			var other = kind;
			return $elm$json$Json$Decode$fail('Unknown value type: ' + other);
		}
	},
	A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string));
var $author$project$Morphir$Elm$Target$CypherOptions = function (a) {
	return {$: 3, a: a};
};
var $author$project$Morphir$Elm$Target$ScalaOptions = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Elm$Target$SpringBootOptions = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Elm$Target$TriplesOptions = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Scala$Backend$Options = {};
var $author$project$Morphir$Graph$Backend$Codec$decodeOptions = $elm$json$Json$Decode$succeed($author$project$Morphir$Scala$Backend$Options);
var $author$project$Morphir$Scala$Backend$Codec$decodeOptions = $elm$json$Json$Decode$succeed($author$project$Morphir$Scala$Backend$Options);
var $author$project$Morphir$SpringBoot$Backend$Codec$decodeOptions = $elm$json$Json$Decode$succeed($author$project$Morphir$Scala$Backend$Options);
var $author$project$Morphir$Elm$Target$decodeOptions = function (gen) {
	_v0$3:
	while (true) {
		if (!gen.$) {
			switch (gen.a) {
				case 'SpringBoot':
					return A2(
						$elm$json$Json$Decode$map,
						function (options) {
							return $author$project$Morphir$Elm$Target$SpringBootOptions(options);
						},
						$author$project$Morphir$SpringBoot$Backend$Codec$decodeOptions);
				case 'triples':
					return A2(
						$elm$json$Json$Decode$map,
						function (options) {
							return $author$project$Morphir$Elm$Target$TriplesOptions(options);
						},
						$author$project$Morphir$Graph$Backend$Codec$decodeOptions);
				case 'cypher':
					return A2(
						$elm$json$Json$Decode$map,
						function (options) {
							return $author$project$Morphir$Elm$Target$CypherOptions(options);
						},
						$author$project$Morphir$Graph$Backend$Codec$decodeOptions);
				default:
					break _v0$3;
			}
		} else {
			break _v0$3;
		}
	}
	return A2(
		$elm$json$Json$Decode$map,
		function (options) {
			return $author$project$Morphir$Elm$Target$ScalaOptions(options);
		},
		$author$project$Morphir$Scala$Backend$Codec$decodeOptions);
};
var $author$project$Morphir$Elm$Frontend$PackageInfo = F2(
	function (name, exposedModules) {
		return {bj: exposedModules, cN: name};
	});
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {eO: index, e$: match, fh: number, f_: submatches};
	});
var $elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{d5: false, e8: false},
		string);
};
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$regex$Regex$never = _Regex_never;
var $elm$core$String$toLower = _String_toLower;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Morphir$IR$Name$fromString = function (string) {
	var wordPattern = A2(
		$elm$core$Maybe$withDefault,
		$elm$regex$Regex$never,
		$elm$regex$Regex$fromString('([a-zA-Z][a-z]*|[0-9]+)'));
	return $author$project$Morphir$IR$Name$fromList(
		A2(
			$elm$core$List$map,
			$elm$core$String$toLower,
			A2(
				$elm$core$List$map,
				function ($) {
					return $.e$;
				},
				A2($elm$regex$Regex$find, wordPattern, string))));
};
var $elm$regex$Regex$split = _Regex_splitAtMost(_Regex_infinity);
var $author$project$Morphir$IR$Path$fromString = function (string) {
	var separatorRegex = A2(
		$elm$core$Maybe$withDefault,
		$elm$regex$Regex$never,
		$elm$regex$Regex$fromString('[^\\w\\s]+'));
	return $author$project$Morphir$IR$Path$fromList(
		A2(
			$elm$core$List$map,
			$author$project$Morphir$IR$Name$fromString,
			A2($elm$regex$Regex$split, separatorRegex, string)));
};
var $author$project$Morphir$Elm$Frontend$Codec$decodePackageInfo = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Morphir$Elm$Frontend$PackageInfo,
	A2(
		$elm$json$Json$Decode$field,
		'name',
		A2($elm$json$Json$Decode$map, $author$project$Morphir$IR$Path$fromString, $elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'exposedModules',
		A2(
			$elm$json$Json$Decode$map,
			$elm$core$Set$fromList,
			$elm$json$Json$Decode$list(
				A2($elm$json$Json$Decode$map, $author$project$Morphir$IR$Path$fromString, $elm$json$Json$Decode$string)))));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Morphir$IR$SDK$packageName = $author$project$Morphir$IR$Path$fromString('Morphir.SDK');
var $author$project$Morphir$IR$SDK$Basics$moduleName = $author$project$Morphir$IR$Path$fromString('Basics');
var $author$project$Morphir$IR$QName$QName = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$IR$QName$fromName = F2(
	function (modulePath, localName) {
		return A2($author$project$Morphir$IR$QName$QName, modulePath, localName);
	});
var $author$project$Morphir$IR$QName$getLocalName = function (_v0) {
	var localName = _v0.b;
	return localName;
};
var $author$project$Morphir$IR$QName$getModulePath = function (_v0) {
	var modulePath = _v0.a;
	return modulePath;
};
var $author$project$Morphir$IR$FQName$fromQName = F2(
	function (packagePath, qName) {
		return A3(
			$author$project$Morphir$IR$FQName$FQName,
			packagePath,
			$author$project$Morphir$IR$QName$getModulePath(qName),
			$author$project$Morphir$IR$QName$getLocalName(qName));
	});
var $author$project$Morphir$IR$SDK$Common$packageName = $author$project$Morphir$IR$Path$fromString('Morphir.SDK');
var $author$project$Morphir$IR$SDK$Common$toFQName = F2(
	function (modulePath, localName) {
		return A2(
			$author$project$Morphir$IR$FQName$fromQName,
			$author$project$Morphir$IR$SDK$Common$packageName,
			A2(
				$author$project$Morphir$IR$QName$fromName,
				modulePath,
				$author$project$Morphir$IR$Name$fromString(localName)));
	});
var $author$project$Morphir$IR$SDK$Basics$boolType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'Bool'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Basics$floatType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'Float'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Basics$intType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'Int'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Basics$neverType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'Never'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Basics$orderType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'Order'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Common$tFun = F2(
	function (argTypes, returnType) {
		var curry = function (args) {
			if (!args.b) {
				return returnType;
			} else {
				var firstArg = args.a;
				var restOfArgs = args.b;
				return A3(
					$author$project$Morphir$IR$Type$Function,
					0,
					firstArg,
					curry(restOfArgs));
			}
		};
		return curry(argTypes);
	});
var $author$project$Morphir$IR$SDK$Common$tVar = function (varName) {
	return A2(
		$author$project$Morphir$IR$Type$Variable,
		0,
		$author$project$Morphir$IR$Name$fromString(varName));
};
var $author$project$Morphir$IR$SDK$Common$vSpec = F3(
	function (name, args, returnType) {
		return _Utils_Tuple2(
			$author$project$Morphir$IR$Name$fromString(name),
			A2(
				$author$project$Morphir$IR$Value$Specification,
				A2(
					$elm$core$List$map,
					function (_v0) {
						var argName = _v0.a;
						var argType = _v0.b;
						return _Utils_Tuple2(
							$author$project$Morphir$IR$Name$fromString(argName),
							argType);
					},
					args),
				returnType));
	});
var $author$project$Morphir$IR$SDK$Basics$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Int'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents an integer value.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil))),
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Float'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a floating-point number.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil))),
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Order'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Represents the relative ordering of two things. The relations are less than, equal to, and greater than.',
					A2(
						$author$project$Morphir$IR$Type$CustomTypeSpecification,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('LT'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('EQ'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('GT'),
								_List_Nil)
							])))),
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Bool'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a boolean value.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil))),
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Never'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'A value that can never happen!',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil)))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'add',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('number')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('number'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'subtract',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('number')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('number'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'multiply',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('number')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('number'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'divide',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'integerDivide',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'power',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('number')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('number'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toFloat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'round',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'floor',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'ceiling',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'truncate',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'modBy',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'remainderBy',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'negate',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('number'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'abs',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('number'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'clamp',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('number')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('number')),
						_Utils_Tuple2(
						'c',
						$author$project$Morphir$IR$SDK$Common$tVar('number'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'isNaN',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'isInfinite',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'sqrt',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'logBase',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'e',
				_List_Nil,
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'pi',
				_List_Nil,
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'cos',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'sin',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'tan',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'acos',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'asin',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'atan',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'atan2',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'degrees',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'radians',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'turns',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toPolar',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						A2(
							$author$project$Morphir$IR$Type$Tuple,
							0,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Basics$floatType(0),
									$author$project$Morphir$IR$SDK$Basics$floatType(0)
								])))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Basics$floatType(0),
							$author$project$Morphir$IR$SDK$Basics$floatType(0)
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromPolar',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						A2(
							$author$project$Morphir$IR$Type$Tuple,
							0,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Basics$floatType(0),
									$author$project$Morphir$IR$SDK$Basics$floatType(0)
								])))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Basics$floatType(0),
							$author$project$Morphir$IR$SDK$Basics$floatType(0)
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'equal',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('eq')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('eq'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'notEqual',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('eq')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('eq'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'lessThan',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'greaterThan',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'lessThanOrEqual',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'greaterThanOrEqual',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'max',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'min',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'compare',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				$author$project$Morphir$IR$SDK$Basics$orderType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'not',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$boolType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'and',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$boolType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$boolType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'or',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$boolType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$boolType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'xor',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$boolType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Basics$boolType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'append',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('appendable')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('appendable'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('appendable')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'identity',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('a'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('a')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'always',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('a')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('b'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('a')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'composeLeft',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'g',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('c'))),
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Common$tFun,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Common$tVar('a')
						]),
					$author$project$Morphir$IR$SDK$Common$tVar('c'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'composeRight',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'g',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('c')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Common$tFun,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Common$tVar('a')
						]),
					$author$project$Morphir$IR$SDK$Common$tVar('c'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'never',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$neverType(0))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('a'))
			]))
};
var $author$project$Morphir$IR$SDK$Char$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Char'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a single character.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil)))
			])),
	gq: function () {
		var valueNames = _List_fromArray(
			['isUpper', 'isLower', 'isAlpha', 'isAlphaNum', 'isDigit', 'isOctDigit', 'isHexDigit', 'toUpper', 'toLower', 'toLocaleUpper', 'toLocaleLower', 'toCode', 'fromCode']);
		var dummyValueSpec = A2(
			$author$project$Morphir$IR$Value$Specification,
			_List_Nil,
			$author$project$Morphir$IR$Type$Unit(0));
		return $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (valueName) {
					return _Utils_Tuple2(
						$author$project$Morphir$IR$Name$fromString(valueName),
						dummyValueSpec);
				},
				valueNames));
	}()
};
var $author$project$Morphir$IR$SDK$Decimal$moduleName = $author$project$Morphir$IR$Path$fromString('Decimal');
var $author$project$Morphir$IR$SDK$Decimal$decimalType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Decimal$moduleName, 'Decimal'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Maybe$moduleName = $author$project$Morphir$IR$Path$fromString('Maybe');
var $author$project$Morphir$IR$SDK$Maybe$maybeType = F2(
	function (attributes, itemType) {
		return A3(
			$author$project$Morphir$IR$Type$Reference,
			attributes,
			A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Maybe$moduleName, 'Maybe'),
			_List_fromArray(
				[itemType]));
	});
var $author$project$Morphir$IR$SDK$String$moduleName = $author$project$Morphir$IR$Path$fromString('String');
var $author$project$Morphir$IR$SDK$String$stringType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$String$moduleName, 'String'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Decimal$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Decimal'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a Decimal.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil)))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromInt',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromFloat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Decimal$decimalType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromString',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'str',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Decimal$decimalType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'hundred',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'thousand',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'million',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'tenth',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'hundredth',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'millionth',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'bps',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toString',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'decimalValue',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toFloat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'd',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$floatType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'add',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'sub',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'negate',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'mul',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'div',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Decimal$decimalType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'divWithDefault',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'default',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Decimal$decimalType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'truncate',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'd',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'round',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'd',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'gt',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'gte',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'eq',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'neq',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'lt',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'lte',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'compare',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$orderType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'abs',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'shiftDecimalLeft',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'shiftDecimalRight',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'zero',
				_List_Nil,
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'one',
				_List_Nil,
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'minusOne',
				_List_Nil,
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0))
			]))
};
var $author$project$Morphir$IR$SDK$Dict$moduleName = $author$project$Morphir$IR$Path$fromString('Dict');
var $author$project$Morphir$IR$SDK$Dict$dictType = F3(
	function (attributes, keyType, valueType) {
		return A3(
			$author$project$Morphir$IR$Type$Reference,
			attributes,
			A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Dict$moduleName, 'dict'),
			_List_fromArray(
				[keyType, valueType]));
	});
var $author$project$Morphir$IR$SDK$List$moduleName = $author$project$Morphir$IR$Path$fromString('List');
var $author$project$Morphir$IR$SDK$List$listType = F2(
	function (attributes, itemType) {
		return A3(
			$author$project$Morphir$IR$Type$Reference,
			attributes,
			A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$List$moduleName, 'List'),
			_List_fromArray(
				[itemType]));
	});
var $author$project$Morphir$IR$SDK$Dict$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Dict'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a dictionary of key-value pairs.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(
						_List_fromArray(
							[
								_List_fromArray(
								['a'])
							]))))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'empty',
				_List_Nil,
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('k'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'singleton',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'key',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Common$tVar('v'))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'insert',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'key',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Common$tVar('v')),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'update',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'key',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									A2(
									$author$project$Morphir$IR$SDK$Maybe$maybeType,
									0,
									$author$project$Morphir$IR$SDK$Common$tVar('v'))
								]),
							A2(
								$author$project$Morphir$IR$SDK$Maybe$maybeType,
								0,
								$author$project$Morphir$IR$SDK$Common$tVar('v')))),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'remove',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'key',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'isEmpty',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'member',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'key',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'get',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'key',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'size',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'keys',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('k'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('k'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'values',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('k'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toList',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('k'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					A2(
						$author$project$Morphir$IR$Type$Tuple,
						0,
						_List_fromArray(
							[
								$author$project$Morphir$IR$SDK$Common$tVar('k'),
								$author$project$Morphir$IR$SDK$Common$tVar('v')
							])))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromList',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							A2(
								$author$project$Morphir$IR$Type$Tuple,
								0,
								_List_fromArray(
									[
										$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
										$author$project$Morphir$IR$SDK$Common$tVar('v')
									]))))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('k'),
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('k'),
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('k'),
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldl',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('k'),
									$author$project$Morphir$IR$SDK$Common$tVar('v'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						'list',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('k'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldr',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('k'),
									$author$project$Morphir$IR$SDK$Common$tVar('v'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						'list',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('k'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'filter',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
									$author$project$Morphir$IR$SDK$Common$tVar('v')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'partition',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
									$author$project$Morphir$IR$SDK$Common$tVar('v')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'dict',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')),
							A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v'))
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'union',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict1',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v'))),
						_Utils_Tuple2(
						'dict2',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'intersect',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict1',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v'))),
						_Utils_Tuple2(
						'dict2',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'diff',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'dict1',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v'))),
						_Utils_Tuple2(
						'dict2',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('v')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Dict$dictType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
					$author$project$Morphir$IR$SDK$Common$tVar('v'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'merge',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'leftOnly',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('result')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('result'))),
						_Utils_Tuple2(
						'both',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('result')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('result'))),
						_Utils_Tuple2(
						'rightOnly',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('result')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('result'))),
						_Utils_Tuple2(
						'dictLeft',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'dictRight',
						A3(
							$author$project$Morphir$IR$SDK$Dict$dictType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'input',
						$author$project$Morphir$IR$SDK$Common$tVar('result'))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('result'))
			]))
};
var $author$project$Morphir$IR$SDK$List$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('List'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a list of values.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(
						_List_fromArray(
							[
								_List_fromArray(
								['a'])
							]))))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'singleton',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('a'))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'repeat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('a'))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'range',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'from',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'to',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Basics$intType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'cons',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'head',
						$author$project$Morphir$IR$SDK$Common$tVar('a')),
						_Utils_Tuple2(
						'tail',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'indexedMap',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Basics$intType(0),
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldl',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldr',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'filter',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'filterMap',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							A2(
								$author$project$Morphir$IR$SDK$Maybe$maybeType,
								0,
								$author$project$Morphir$IR$SDK$Common$tVar('b')))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'length',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'reverse',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'member',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						$author$project$Morphir$IR$SDK$Common$tVar('a')),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'all',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'any',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'maximum',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'minimum',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'sum',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('number')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'product',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('number')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('number')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'append',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'l1',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'l2',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'concat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'lists',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							A2(
								$author$project$Morphir$IR$SDK$List$listType,
								0,
								$author$project$Morphir$IR$SDK$Common$tVar('a'))))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'concatMap',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							A2(
								$author$project$Morphir$IR$SDK$List$listType,
								0,
								$author$project$Morphir$IR$SDK$Common$tVar('b')))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'intersperse',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('a')),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map2',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'list1',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'list2',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map3',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'list1',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'list2',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'list3',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('c')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map4',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c'),
									$author$project$Morphir$IR$SDK$Common$tVar('d')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'list1',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'list2',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'list3',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('c'))),
						_Utils_Tuple2(
						'list4',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('d')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map5',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c'),
									$author$project$Morphir$IR$SDK$Common$tVar('d'),
									$author$project$Morphir$IR$SDK$Common$tVar('e')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'list1',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'list2',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'list3',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('c'))),
						_Utils_Tuple2(
						'list4',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('d'))),
						_Utils_Tuple2(
						'list5',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('e')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'sort',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'sortBy',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'sortWith',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Basics$orderType(0))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'isEmpty',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'head',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'tail',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					A2(
						$author$project$Morphir$IR$SDK$List$listType,
						0,
						$author$project$Morphir$IR$SDK$Common$tVar('a')))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'take',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'drop',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'partition',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')),
							A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'unzip',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							A2(
								$author$project$Morphir$IR$Type$Tuple,
								0,
								_List_fromArray(
									[
										$author$project$Morphir$IR$SDK$Common$tVar('a'),
										$author$project$Morphir$IR$SDK$Common$tVar('b')
									]))))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')),
							A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b'))
						])))
			]))
};
var $author$project$Morphir$IR$SDK$LocalDate$moduleName = $author$project$Morphir$IR$Path$fromString('LocalDate');
var $author$project$Morphir$IR$SDK$LocalDate$localDateType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$LocalDate$moduleName, 'LocalDate'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$LocalDate$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('LocalDate'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a date concept.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil)))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromISO',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'iso',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromParts',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'year',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'month',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'day',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'diffInDays',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'date1',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0)),
						_Utils_Tuple2(
						'date2',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'diffInWeeks',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'date1',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0)),
						_Utils_Tuple2(
						'date2',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'diffInMonths',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'date1',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0)),
						_Utils_Tuple2(
						'date2',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'diffInYears',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'date1',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0)),
						_Utils_Tuple2(
						'date2',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'addDays',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'offset',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'startDate',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$LocalDate$localDateType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'addWeeks',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'offset',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'startDate',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$LocalDate$localDateType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'addMonths',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'offset',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'startDate',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$LocalDate$localDateType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'addYears',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'offset',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'startDate',
						$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
					]),
				$author$project$Morphir$IR$SDK$LocalDate$localDateType(0))
			]))
};
var $author$project$Morphir$IR$SDK$Maybe$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Maybe'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents an optional value.',
					A2(
						$author$project$Morphir$IR$Type$CustomTypeSpecification,
						_List_fromArray(
							[
								$author$project$Morphir$IR$Name$fromString('a')
							]),
						_List_fromArray(
							[
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('Just'),
								_List_fromArray(
									[
										_Utils_Tuple2(
										_List_fromArray(
											['value']),
										A2(
											$author$project$Morphir$IR$Type$Variable,
											0,
											$author$project$Morphir$IR$Name$fromString('a')))
									])),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('Nothing'),
								_List_Nil)
							]))))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'andThen',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							A2(
								$author$project$Morphir$IR$SDK$Maybe$maybeType,
								0,
								$author$project$Morphir$IR$SDK$Common$tVar('b')))),
						_Utils_Tuple2(
						'maybe',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'maybe',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map2',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'maybe1',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'maybe2',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map3',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'maybe1',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'maybe2',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'maybe3',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('c')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map4',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c'),
									$author$project$Morphir$IR$SDK$Common$tVar('d')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'maybe1',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'maybe2',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'maybe3',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('c'))),
						_Utils_Tuple2(
						'maybe4',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('d')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map5',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c'),
									$author$project$Morphir$IR$SDK$Common$tVar('d'),
									$author$project$Morphir$IR$SDK$Common$tVar('e')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'maybe1',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'maybe2',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'maybe3',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('c'))),
						_Utils_Tuple2(
						'maybe4',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('d'))),
						_Utils_Tuple2(
						'maybe5',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('e')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'withDefault',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'default',
						$author$project$Morphir$IR$SDK$Common$tVar('a')),
						_Utils_Tuple2(
						'maybe',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('a'))
			]))
};
var $author$project$Morphir$IR$SDK$Month$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Month'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents an month concept.',
					A2(
						$author$project$Morphir$IR$Type$CustomTypeSpecification,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('January'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('February'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('March'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('April'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('May'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('June'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('July'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('August'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('September'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('October'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('November'),
								_List_Nil),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('December'),
								_List_Nil)
							]))))
			])),
	gq: $elm$core$Dict$empty
};
var $author$project$Morphir$IR$SDK$Number$moduleName = $author$project$Morphir$IR$Path$fromString('Number');
var $author$project$Morphir$IR$SDK$Number$divisionByZeroType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Number$moduleName, 'DivisionByZero'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Number$numberType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Number$moduleName, 'Number'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$Result$moduleName = $author$project$Morphir$IR$Path$fromString('Result');
var $author$project$Morphir$IR$SDK$Result$resultType = F3(
	function (attributes, errorType, itemType) {
		return A3(
			$author$project$Morphir$IR$Type$Reference,
			attributes,
			A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Result$moduleName, 'result'),
			_List_fromArray(
				[errorType, itemType]));
	});
var $author$project$Morphir$IR$SDK$Number$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Decimal'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a Decimal.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil)))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromInt',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'equal',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'notEqual',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'lessThan',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'lessThanOrEqual',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'greaterThan',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'greaterThanOrEqual',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'add',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'subtract',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'multiply',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'divide',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0)),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Number$divisionByZeroType(0),
					$author$project$Morphir$IR$SDK$Number$numberType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'abs',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'negate',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'reciprocal',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'coerceToDecimal',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'default',
						$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
						_Utils_Tuple2(
						'number',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Decimal$decimalType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toDecimal',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'number',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Decimal$decimalType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toFractionalString',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'num',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'simplify',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Number$numberType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'isSimplified',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Number$numberType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'zero',
				_List_Nil,
				$author$project$Morphir$IR$SDK$Number$numberType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'one',
				_List_Nil,
				$author$project$Morphir$IR$SDK$Number$numberType(0))
			]))
};
var $author$project$Morphir$IR$SDK$Regex$moduleSpec = {
	gn: $elm$core$Dict$empty,
	gq: function () {
		var valueNames = _List_fromArray(
			['fromString', 'fromStringWith', 'never', 'contains', 'split', 'find', 'replace', 'splitAtMost', 'findAtMost', 'replaceAtMost']);
		var dummyValueSpec = A2(
			$author$project$Morphir$IR$Value$Specification,
			_List_Nil,
			$author$project$Morphir$IR$Type$Unit(0));
		return $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (valueName) {
					return _Utils_Tuple2(
						$author$project$Morphir$IR$Name$fromString(valueName),
						dummyValueSpec);
				},
				valueNames));
	}()
};
var $author$project$Morphir$IR$SDK$Result$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Result'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents the result of a computation that can either succeed or fail.',
					A2(
						$author$project$Morphir$IR$Type$CustomTypeSpecification,
						_List_fromArray(
							[
								$author$project$Morphir$IR$Name$fromString('e'),
								$author$project$Morphir$IR$Name$fromString('a')
							]),
						_List_fromArray(
							[
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('Ok'),
								_List_fromArray(
									[
										_Utils_Tuple2(
										$author$project$Morphir$IR$Name$fromString('value'),
										A2(
											$author$project$Morphir$IR$Type$Variable,
											0,
											$author$project$Morphir$IR$Name$fromString('a')))
									])),
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('Err'),
								_List_fromArray(
									[
										_Utils_Tuple2(
										$author$project$Morphir$IR$Name$fromString('error'),
										A2(
											$author$project$Morphir$IR$Type$Variable,
											0,
											$author$project$Morphir$IR$Name$fromString('e')))
									]))
							]))))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'andThen',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							A3(
								$author$project$Morphir$IR$SDK$Result$resultType,
								0,
								$author$project$Morphir$IR$SDK$Common$tVar('x'),
								$author$project$Morphir$IR$SDK$Common$tVar('b')))),
						_Utils_Tuple2(
						'result',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('x'),
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'result',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('x'),
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map2',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'result1',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('b')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('x'),
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map3',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'result1',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('c')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('x'),
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map4',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c'),
									$author$project$Morphir$IR$SDK$Common$tVar('d')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'result1',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('c'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('d')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('x'),
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map5',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b'),
									$author$project$Morphir$IR$SDK$Common$tVar('c'),
									$author$project$Morphir$IR$SDK$Common$tVar('d'),
									$author$project$Morphir$IR$SDK$Common$tVar('e')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('r'))),
						_Utils_Tuple2(
						'result1',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('c'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('d'))),
						_Utils_Tuple2(
						'result2',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('e')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('x'),
					$author$project$Morphir$IR$SDK$Common$tVar('r'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'withDefault',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'default',
						$author$project$Morphir$IR$SDK$Common$tVar('a')),
						_Utils_Tuple2(
						'result',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('a')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toMaybe',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'result',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromMaybe',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'error',
						$author$project$Morphir$IR$SDK$Common$tVar('x')),
						_Utils_Tuple2(
						'maybe',
						A2(
							$author$project$Morphir$IR$SDK$Maybe$maybeType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('x'),
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'mapError',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('x')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('y'))),
						_Utils_Tuple2(
						'result',
						A3(
							$author$project$Morphir$IR$SDK$Result$resultType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Result$resultType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('y'),
					$author$project$Morphir$IR$SDK$Common$tVar('a')))
			]))
};
var $author$project$Morphir$IR$SDK$Rule$moduleName = $author$project$Morphir$IR$Path$fromString('Rule');
var $author$project$Morphir$IR$SDK$Rule$ruleType = F3(
	function (attributes, itemType1, itemType2) {
		return A3(
			$author$project$Morphir$IR$Type$Reference,
			attributes,
			A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Rule$moduleName, 'Rule'),
			_List_fromArray(
				[itemType1, itemType2]));
	});
var $author$project$Morphir$IR$SDK$Rule$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Rule'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents an rule.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(
						_List_fromArray(
							[
								_List_fromArray(
								['a', 'b'])
							]))))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'chain',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'rules',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							A3(
								$author$project$Morphir$IR$SDK$Rule$ruleType,
								0,
								$author$project$Morphir$IR$SDK$Common$tVar('a'),
								$author$project$Morphir$IR$SDK$Common$tVar('b'))))
					]),
				A3(
					$author$project$Morphir$IR$SDK$Rule$ruleType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'),
					$author$project$Morphir$IR$SDK$Common$tVar('b'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'any',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Common$tVar('a'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'is',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						$author$project$Morphir$IR$SDK$Common$tVar('a')),
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Common$tVar('a'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'anyOf',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Common$tVar('a'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'noneOf',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a'))),
						_Utils_Tuple2(
						'value',
						$author$project$Morphir$IR$SDK$Common$tVar('a'))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0))
			]))
};
var $author$project$Morphir$IR$SDK$Set$moduleName = $author$project$Morphir$IR$Path$fromString('Set');
var $author$project$Morphir$IR$SDK$Set$setType = F2(
	function (attributes, itemType) {
		return A3(
			$author$project$Morphir$IR$Type$Reference,
			attributes,
			A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Set$moduleName, 'set'),
			_List_fromArray(
				[itemType]));
	});
var $author$project$Morphir$IR$SDK$Set$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('Set'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a set.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(
						_List_fromArray(
							[
								_List_fromArray(
								['a'])
							]))))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'empty',
				_List_Nil,
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'singleton',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'insert',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'remove',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'isEmpty',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'member',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'size',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toList',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('a'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromList',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('comparable2'))),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable2'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldl',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldr',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('a')))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'filter',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'partition',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('comparable')
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						'set',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')),
							A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'))
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'union',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'set1',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
						_Utils_Tuple2(
						'set2',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'intersect',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'set1',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
						_Utils_Tuple2(
						'set2',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'diff',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'set1',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable'))),
						_Utils_Tuple2(
						'set2',
						A2(
							$author$project$Morphir$IR$SDK$Set$setType,
							0,
							$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Set$setType,
					0,
					$author$project$Morphir$IR$SDK$Common$tVar('comparable')))
			]))
};
var $author$project$Morphir$IR$SDK$StatefulApp$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('StatefulApp'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a stateful app.',
					A2(
						$author$project$Morphir$IR$Type$CustomTypeSpecification,
						_List_fromArray(
							[
								$author$project$Morphir$IR$Name$fromString('k'),
								$author$project$Morphir$IR$Name$fromString('c'),
								$author$project$Morphir$IR$Name$fromString('s'),
								$author$project$Morphir$IR$Name$fromString('e')
							]),
						_List_fromArray(
							[
								A2(
								$author$project$Morphir$IR$Type$Constructor,
								$author$project$Morphir$IR$Name$fromString('StatefulApp'),
								_List_fromArray(
									[
										_Utils_Tuple2(
										$author$project$Morphir$IR$Name$fromString('logic'),
										A3(
											$author$project$Morphir$IR$Type$Function,
											0,
											A2(
												$author$project$Morphir$IR$SDK$Maybe$maybeType,
												0,
												A2(
													$author$project$Morphir$IR$Type$Variable,
													0,
													$author$project$Morphir$IR$Name$fromString('s'))),
											A3(
												$author$project$Morphir$IR$Type$Function,
												0,
												A2(
													$author$project$Morphir$IR$Type$Variable,
													0,
													$author$project$Morphir$IR$Name$fromString('c')),
												A2(
													$author$project$Morphir$IR$Type$Tuple,
													0,
													_List_fromArray(
														[
															A2(
															$author$project$Morphir$IR$SDK$Maybe$maybeType,
															0,
															A2(
																$author$project$Morphir$IR$Type$Variable,
																0,
																$author$project$Morphir$IR$Name$fromString('s'))),
															A2(
															$author$project$Morphir$IR$Type$Variable,
															0,
															$author$project$Morphir$IR$Name$fromString('e'))
														])))))
									]))
							]))))
			])),
	gq: $elm$core$Dict$empty
};
var $author$project$Morphir$IR$SDK$Char$moduleName = $author$project$Morphir$IR$Path$fromString('Char');
var $author$project$Morphir$IR$SDK$Char$charType = function (attributes) {
	return A3(
		$author$project$Morphir$IR$Type$Reference,
		attributes,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Char$moduleName, 'Char'),
		_List_Nil);
};
var $author$project$Morphir$IR$SDK$String$moduleSpec = {
	gn: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Morphir$IR$Name$fromString('String'),
				A2(
					$author$project$Morphir$IR$Documented$Documented,
					'Type that represents a string of characters.',
					$author$project$Morphir$IR$Type$OpaqueTypeSpecification(_List_Nil)))
			])),
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'isEmpty',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'length',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$intType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'reverse',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'repeat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'replace',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'match',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						'replacement',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'append',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's1',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's2',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'concat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$String$stringType(0)))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'split',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'sep',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$String$stringType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'join',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'sep',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						'list',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$String$stringType(0)))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'words',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$String$stringType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'lines',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$String$stringType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'slice',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'start',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						'end',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'left',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'right',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'dropLeft',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'dropRight',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'contains',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'startsWith',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'endsWith',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'indexes',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Basics$intType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'indices',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ref',
						$author$project$Morphir$IR$SDK$String$stringType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Basics$intType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toInt',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Basics$intType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromInt',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$intType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toFloat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					$author$project$Morphir$IR$SDK$Basics$floatType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromFloat',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Basics$floatType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromChar',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ch',
						$author$project$Morphir$IR$SDK$Char$charType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'cons',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'ch',
						$author$project$Morphir$IR$SDK$Char$charType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'uncons',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$Maybe$maybeType,
					0,
					A2(
						$author$project$Morphir$IR$Type$Tuple,
						0,
						_List_fromArray(
							[
								$author$project$Morphir$IR$SDK$Char$charType(0),
								$author$project$Morphir$IR$SDK$String$stringType(0)
							])))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toList',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				A2(
					$author$project$Morphir$IR$SDK$List$listType,
					0,
					$author$project$Morphir$IR$SDK$Char$charType(0))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'fromList',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						A2(
							$author$project$Morphir$IR$SDK$List$listType,
							0,
							$author$project$Morphir$IR$SDK$Char$charType(0)))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toUpper',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'toLower',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'pad',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'padLeft',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'padRight',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'n',
						$author$project$Morphir$IR$SDK$Basics$intType(0)),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'trim',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'trimLeft',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'trimRight',
				_List_fromArray(
					[
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'map',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Char$charType(0)
								]),
							$author$project$Morphir$IR$SDK$Char$charType(0))),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'filter',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Char$charType(0)
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$String$stringType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldl',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Char$charType(0),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'foldr',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Char$charType(0),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('b'))),
						_Utils_Tuple2(
						'z',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'any',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Char$charType(0)
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0)),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'all',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Char$charType(0)
								]),
							$author$project$Morphir$IR$SDK$Basics$boolType(0))),
						_Utils_Tuple2(
						's',
						$author$project$Morphir$IR$SDK$String$stringType(0))
					]),
				$author$project$Morphir$IR$SDK$Basics$boolType(0))
			]))
};
var $author$project$Morphir$IR$SDK$Tuple$moduleSpec = {
	gn: $elm$core$Dict$empty,
	gq: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'pair',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'a',
						$author$project$Morphir$IR$SDK$Common$tVar('b')),
						_Utils_Tuple2(
						'b',
						$author$project$Morphir$IR$SDK$Common$tVar('b'))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Common$tVar('a'),
							$author$project$Morphir$IR$SDK$Common$tVar('b')
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'first',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tuple',
						A2(
							$author$project$Morphir$IR$Type$Tuple,
							0,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								])))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('a')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'second',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tuple',
						A2(
							$author$project$Morphir$IR$Type$Tuple,
							0,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								])))
					]),
				$author$project$Morphir$IR$SDK$Common$tVar('b')),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'mapFirst',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('x'))),
						_Utils_Tuple2(
						'tuple',
						A2(
							$author$project$Morphir$IR$Type$Tuple,
							0,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								])))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('b')
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'mapSecond',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('y'))),
						_Utils_Tuple2(
						'tuple',
						A2(
							$author$project$Morphir$IR$Type$Tuple,
							0,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								])))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Common$tVar('a'),
							$author$project$Morphir$IR$SDK$Common$tVar('y')
						]))),
				A3(
				$author$project$Morphir$IR$SDK$Common$vSpec,
				'mapBoth',
				_List_fromArray(
					[
						_Utils_Tuple2(
						'f',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('x'))),
						_Utils_Tuple2(
						'g',
						A2(
							$author$project$Morphir$IR$SDK$Common$tFun,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								]),
							$author$project$Morphir$IR$SDK$Common$tVar('y'))),
						_Utils_Tuple2(
						'tuple',
						A2(
							$author$project$Morphir$IR$Type$Tuple,
							0,
							_List_fromArray(
								[
									$author$project$Morphir$IR$SDK$Common$tVar('a'),
									$author$project$Morphir$IR$SDK$Common$tVar('b')
								])))
					]),
				A2(
					$author$project$Morphir$IR$Type$Tuple,
					0,
					_List_fromArray(
						[
							$author$project$Morphir$IR$SDK$Common$tVar('x'),
							$author$project$Morphir$IR$SDK$Common$tVar('y')
						])))
			]))
};
var $author$project$Morphir$IR$SDK$packageSpec = {
	e7: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['basics'])
					]),
				$author$project$Morphir$IR$SDK$Basics$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['char'])
					]),
				$author$project$Morphir$IR$SDK$Char$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['dict'])
					]),
				$author$project$Morphir$IR$SDK$Dict$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['set'])
					]),
				$author$project$Morphir$IR$SDK$Set$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['string'])
					]),
				$author$project$Morphir$IR$SDK$String$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['local', 'date'])
					]),
				$author$project$Morphir$IR$SDK$LocalDate$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['maybe'])
					]),
				$author$project$Morphir$IR$SDK$Maybe$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['month'])
					]),
				$author$project$Morphir$IR$SDK$Month$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['result'])
					]),
				$author$project$Morphir$IR$SDK$Result$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['list'])
					]),
				$author$project$Morphir$IR$SDK$List$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['tuple'])
					]),
				$author$project$Morphir$IR$SDK$Tuple$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['regex'])
					]),
				$author$project$Morphir$IR$SDK$Regex$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['stateful', 'app'])
					]),
				$author$project$Morphir$IR$SDK$StatefulApp$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['rule'])
					]),
				$author$project$Morphir$IR$SDK$Rule$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['decimal'])
					]),
				$author$project$Morphir$IR$SDK$Decimal$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['int'])
					]),
				$author$project$Morphir$IR$SDK$Decimal$moduleSpec),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['number'])
					]),
				$author$project$Morphir$IR$SDK$Number$moduleSpec)
			]))
};
var $author$project$Morphir$Elm$Frontend$defaultDependencies = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2($author$project$Morphir$IR$SDK$packageName, $author$project$Morphir$IR$SDK$packageSpec)
		]));
var $author$project$Morphir$IR$AccessControlled$withPublicAccess = function (ac) {
	var _v0 = ac.b0;
	if (!_v0) {
		return $elm$core$Maybe$Just(ac.aT);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Morphir$IR$Type$definitionToSpecification = function (def) {
	if (!def.$) {
		var params = def.a;
		var exp = def.b;
		return A2($author$project$Morphir$IR$Type$TypeAliasSpecification, params, exp);
	} else {
		var params = def.a;
		var accessControlledCtors = def.b;
		var _v1 = $author$project$Morphir$IR$AccessControlled$withPublicAccess(accessControlledCtors);
		if (!_v1.$) {
			var ctors = _v1.a;
			return A2($author$project$Morphir$IR$Type$CustomTypeSpecification, params, ctors);
		} else {
			return $author$project$Morphir$IR$Type$OpaqueTypeSpecification(params);
		}
	}
};
var $author$project$Morphir$IR$Value$definitionToSpecification = function (def) {
	return {
		bo: A2(
			$elm$core$List$map,
			function (_v0) {
				var name = _v0.a;
				var tpe = _v0.c;
				return _Utils_Tuple2(name, tpe);
			},
			def.a3),
		bN: def.fq
	};
};
var $author$project$Morphir$IR$Documented$map = F2(
	function (f, d) {
		return A2(
			$author$project$Morphir$IR$Documented$Documented,
			d.en,
			f(d.aT));
	});
var $author$project$Morphir$IR$AccessControlled$withPrivateAccess = function (ac) {
	var _v0 = ac.b0;
	if (!_v0) {
		return ac.aT;
	} else {
		return ac.aT;
	}
};
var $author$project$Morphir$IR$Module$definitionToSpecificationWithPrivate = function (def) {
	return {
		gn: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var path = _v0.a;
					var accessControlledType = _v0.b;
					return _Utils_Tuple2(
						path,
						A2(
							$author$project$Morphir$IR$Documented$map,
							$author$project$Morphir$IR$Type$definitionToSpecification,
							$author$project$Morphir$IR$AccessControlled$withPrivateAccess(accessControlledType)));
				},
				$elm$core$Dict$toList(def.gn))),
		gq: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (_v1) {
					var path = _v1.a;
					var accessControlledValue = _v1.b;
					return _Utils_Tuple2(
						path,
						$author$project$Morphir$IR$Value$definitionToSpecification(
							$author$project$Morphir$IR$AccessControlled$withPrivateAccess(accessControlledValue)));
				},
				$elm$core$Dict$toList(def.gq)))
	};
};
var $author$project$Morphir$IR$Package$definitionToSpecificationWithPrivate = function (def) {
	return {
		e7: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var path = _v0.a;
					var accessControlledModule = _v0.b;
					return _Utils_Tuple2(
						path,
						$author$project$Morphir$IR$Module$definitionToSpecificationWithPrivate(
							$author$project$Morphir$IR$AccessControlled$withPrivateAccess(accessControlledModule)));
				},
				$elm$core$Dict$toList(def.e7)))
	};
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Morphir$IR$AccessControlled$Codec$encodeAccessControlled = F2(
	function (encodeValue, ac) {
		var _v0 = ac.b0;
		if (!_v0) {
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('public'),
						encodeValue(ac.aT)
					]));
		} else {
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('private'),
						encodeValue(ac.aT)
					]));
		}
	});
var $author$project$Morphir$IR$Name$toList = function (words) {
	return words;
};
var $author$project$Morphir$IR$Name$Codec$encodeName = function (name) {
	return A2(
		$elm$json$Json$Encode$list,
		$elm$json$Json$Encode$string,
		$author$project$Morphir$IR$Name$toList(name));
};
var $author$project$Morphir$IR$Path$toList = function (names) {
	return names;
};
var $author$project$Morphir$IR$Path$Codec$encodePath = function (path) {
	return A2(
		$elm$json$Json$Encode$list,
		$author$project$Morphir$IR$Name$Codec$encodeName,
		$author$project$Morphir$IR$Path$toList(path));
};
var $author$project$Morphir$IR$FQName$Codec$encodeFQName = function (_v0) {
	var packagePath = _v0.a;
	var modulePath = _v0.b;
	var localName = _v0.c;
	return A2(
		$elm$json$Json$Encode$list,
		$elm$core$Basics$identity,
		_List_fromArray(
			[
				$author$project$Morphir$IR$Path$Codec$encodePath(packagePath),
				$author$project$Morphir$IR$Path$Codec$encodePath(modulePath),
				$author$project$Morphir$IR$Name$Codec$encodeName(localName)
			]));
};
var $author$project$Morphir$IR$Type$Codec$encodeField = F2(
	function (encodeAttributes, field) {
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$author$project$Morphir$IR$Name$Codec$encodeName(field.cN),
					A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, field.gl)
				]));
	});
var $author$project$Morphir$IR$Type$Codec$encodeType = F2(
	function (encodeAttributes, tpe) {
		switch (tpe.$) {
			case 0:
				var a = tpe.a;
				var name = tpe.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('variable'),
							encodeAttributes(a),
							$author$project$Morphir$IR$Name$Codec$encodeName(name)
						]));
			case 1:
				var a = tpe.a;
				var typeName = tpe.b;
				var typeParameters = tpe.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('reference'),
							encodeAttributes(a),
							$author$project$Morphir$IR$FQName$Codec$encodeFQName(typeName),
							A2(
							$elm$json$Json$Encode$list,
							$author$project$Morphir$IR$Type$Codec$encodeType(encodeAttributes),
							typeParameters)
						]));
			case 2:
				var a = tpe.a;
				var elementTypes = tpe.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('tuple'),
							encodeAttributes(a),
							A2(
							$elm$json$Json$Encode$list,
							$author$project$Morphir$IR$Type$Codec$encodeType(encodeAttributes),
							elementTypes)
						]));
			case 3:
				var a = tpe.a;
				var fieldTypes = tpe.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('record'),
							encodeAttributes(a),
							A2(
							$elm$json$Json$Encode$list,
							$author$project$Morphir$IR$Type$Codec$encodeField(encodeAttributes),
							fieldTypes)
						]));
			case 4:
				var a = tpe.a;
				var variableName = tpe.b;
				var fieldTypes = tpe.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('extensible_record'),
							encodeAttributes(a),
							$author$project$Morphir$IR$Name$Codec$encodeName(variableName),
							A2(
							$elm$json$Json$Encode$list,
							$author$project$Morphir$IR$Type$Codec$encodeField(encodeAttributes),
							fieldTypes)
						]));
			case 5:
				var a = tpe.a;
				var argumentType = tpe.b;
				var returnType = tpe.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('function'),
							encodeAttributes(a),
							A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, argumentType),
							A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, returnType)
						]));
			default:
				var a = tpe.a;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('unit'),
							encodeAttributes(a)
						]));
		}
	});
var $author$project$Morphir$IR$Type$Codec$encodeConstructors = F2(
	function (encodeAttributes, ctors) {
		return A2(
			$elm$json$Json$Encode$list,
			function (_v0) {
				var ctorName = _v0.a;
				var ctorArgs = _v0.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('constructor'),
							$author$project$Morphir$IR$Name$Codec$encodeName(ctorName),
							A2(
							$elm$json$Json$Encode$list,
							function (_v1) {
								var argName = _v1.a;
								var argType = _v1.b;
								return A2(
									$elm$json$Json$Encode$list,
									$elm$core$Basics$identity,
									_List_fromArray(
										[
											$author$project$Morphir$IR$Name$Codec$encodeName(argName),
											A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, argType)
										]));
							},
							ctorArgs)
						]));
			},
			ctors);
	});
var $author$project$Morphir$IR$Type$Codec$encodeDefinition = F2(
	function (encodeAttributes, def) {
		if (!def.$) {
			var params = def.a;
			var exp = def.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('type_alias_definition'),
						A2($elm$json$Json$Encode$list, $author$project$Morphir$IR$Name$Codec$encodeName, params),
						A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, exp)
					]));
		} else {
			var params = def.a;
			var ctors = def.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('custom_type_definition'),
						A2($elm$json$Json$Encode$list, $author$project$Morphir$IR$Name$Codec$encodeName, params),
						A2(
						$author$project$Morphir$IR$AccessControlled$Codec$encodeAccessControlled,
						$author$project$Morphir$IR$Type$Codec$encodeConstructors(encodeAttributes),
						ctors)
					]));
		}
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$json$Json$Encode$float = _Json_wrap;
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$Morphir$IR$Literal$Codec$encodeLiteral = function (l) {
	switch (l.$) {
		case 0:
			var v = l.a;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('bool_literal'),
						$elm$json$Json$Encode$bool(v)
					]));
		case 1:
			var v = l.a;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('char_literal'),
						$elm$json$Json$Encode$string(
						$elm$core$String$fromChar(v))
					]));
		case 2:
			var v = l.a;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('string_literal'),
						$elm$json$Json$Encode$string(v)
					]));
		case 3:
			var v = l.a;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('int_literal'),
						$elm$json$Json$Encode$int(v)
					]));
		default:
			var v = l.a;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string('float_literal'),
						$elm$json$Json$Encode$float(v)
					]));
	}
};
var $author$project$Morphir$IR$Value$Codec$encodePattern = F2(
	function (encodeAttributes, pattern) {
		switch (pattern.$) {
			case 0:
				var a = pattern.a;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('wildcard_pattern'),
							encodeAttributes(a)
						]));
			case 1:
				var a = pattern.a;
				var p = pattern.b;
				var name = pattern.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('as_pattern'),
							encodeAttributes(a),
							A2($author$project$Morphir$IR$Value$Codec$encodePattern, encodeAttributes, p),
							$author$project$Morphir$IR$Name$Codec$encodeName(name)
						]));
			case 2:
				var a = pattern.a;
				var elementPatterns = pattern.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('tuple_pattern'),
							encodeAttributes(a),
							A2(
							$elm$json$Json$Encode$list,
							$author$project$Morphir$IR$Value$Codec$encodePattern(encodeAttributes),
							elementPatterns)
						]));
			case 3:
				var a = pattern.a;
				var constructorName = pattern.b;
				var argumentPatterns = pattern.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('constructor_pattern'),
							encodeAttributes(a),
							$author$project$Morphir$IR$FQName$Codec$encodeFQName(constructorName),
							A2(
							$elm$json$Json$Encode$list,
							$author$project$Morphir$IR$Value$Codec$encodePattern(encodeAttributes),
							argumentPatterns)
						]));
			case 4:
				var a = pattern.a;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('empty_list_pattern'),
							encodeAttributes(a)
						]));
			case 5:
				var a = pattern.a;
				var headPattern = pattern.b;
				var tailPattern = pattern.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('head_tail_pattern'),
							encodeAttributes(a),
							A2($author$project$Morphir$IR$Value$Codec$encodePattern, encodeAttributes, headPattern),
							A2($author$project$Morphir$IR$Value$Codec$encodePattern, encodeAttributes, tailPattern)
						]));
			case 6:
				var a = pattern.a;
				var value = pattern.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('literal_pattern'),
							encodeAttributes(a),
							$author$project$Morphir$IR$Literal$Codec$encodeLiteral(value)
						]));
			default:
				var a = pattern.a;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('unit_pattern'),
							encodeAttributes(a)
						]));
		}
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $author$project$Morphir$IR$Value$Codec$encodeDefinition = F3(
	function (encodeTypeAttributes, encodeValueAttributes, def) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'inputTypes',
					A2(
						$elm$json$Json$Encode$list,
						function (_v5) {
							var argName = _v5.a;
							var a = _v5.b;
							var argType = _v5.c;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$core$Basics$identity,
								_List_fromArray(
									[
										$author$project$Morphir$IR$Name$Codec$encodeName(argName),
										encodeValueAttributes(a),
										A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeTypeAttributes, argType)
									]));
						},
						def.a3)),
					_Utils_Tuple2(
					'outputType',
					A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeTypeAttributes, def.fq)),
					_Utils_Tuple2(
					'body',
					A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, def.dV))
				]));
	});
var $author$project$Morphir$IR$Value$Codec$encodeValue = F3(
	function (encodeTypeAttributes, encodeValueAttributes, v) {
		switch (v.$) {
			case 0:
				var a = v.a;
				var value = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('literal'),
							encodeValueAttributes(a),
							$author$project$Morphir$IR$Literal$Codec$encodeLiteral(value)
						]));
			case 1:
				var a = v.a;
				var fullyQualifiedName = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('constructor'),
							encodeValueAttributes(a),
							$author$project$Morphir$IR$FQName$Codec$encodeFQName(fullyQualifiedName)
						]));
			case 2:
				var a = v.a;
				var elements = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('tuple'),
							encodeValueAttributes(a),
							A2(
							$elm$json$Json$Encode$list,
							A2($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes),
							elements)
						]));
			case 3:
				var a = v.a;
				var items = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('list'),
							encodeValueAttributes(a),
							A2(
							$elm$json$Json$Encode$list,
							A2($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes),
							items)
						]));
			case 4:
				var a = v.a;
				var fields = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('record'),
							encodeValueAttributes(a),
							A2(
							$elm$json$Json$Encode$list,
							function (_v1) {
								var fieldName = _v1.a;
								var fieldValue = _v1.b;
								return A2(
									$elm$json$Json$Encode$list,
									$elm$core$Basics$identity,
									_List_fromArray(
										[
											$author$project$Morphir$IR$Name$Codec$encodeName(fieldName),
											A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, fieldValue)
										]));
							},
							fields)
						]));
			case 5:
				var a = v.a;
				var name = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('variable'),
							encodeValueAttributes(a),
							$author$project$Morphir$IR$Name$Codec$encodeName(name)
						]));
			case 6:
				var a = v.a;
				var fullyQualifiedName = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('reference'),
							encodeValueAttributes(a),
							$author$project$Morphir$IR$FQName$Codec$encodeFQName(fullyQualifiedName)
						]));
			case 7:
				var a = v.a;
				var subjectValue = v.b;
				var fieldName = v.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('field'),
							encodeValueAttributes(a),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, subjectValue),
							$author$project$Morphir$IR$Name$Codec$encodeName(fieldName)
						]));
			case 8:
				var a = v.a;
				var fieldName = v.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('field_function'),
							encodeValueAttributes(a),
							$author$project$Morphir$IR$Name$Codec$encodeName(fieldName)
						]));
			case 9:
				var a = v.a;
				var _function = v.b;
				var argument = v.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('apply'),
							encodeValueAttributes(a),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, _function),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, argument)
						]));
			case 10:
				var a = v.a;
				var argumentPattern = v.b;
				var body = v.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('lambda'),
							encodeValueAttributes(a),
							A2($author$project$Morphir$IR$Value$Codec$encodePattern, encodeValueAttributes, argumentPattern),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, body)
						]));
			case 11:
				var a = v.a;
				var valueName = v.b;
				var valueDefinition = v.c;
				var inValue = v.d;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('let_definition'),
							encodeValueAttributes(a),
							$author$project$Morphir$IR$Name$Codec$encodeName(valueName),
							A3($author$project$Morphir$IR$Value$Codec$encodeDefinition, encodeTypeAttributes, encodeValueAttributes, valueDefinition),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, inValue)
						]));
			case 12:
				var a = v.a;
				var valueDefinitions = v.b;
				var inValue = v.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('let_recursion'),
							encodeValueAttributes(a),
							A2(
							$elm$json$Json$Encode$list,
							function (_v2) {
								var name = _v2.a;
								var def = _v2.b;
								return A2(
									$elm$json$Json$Encode$list,
									$elm$core$Basics$identity,
									_List_fromArray(
										[
											$author$project$Morphir$IR$Name$Codec$encodeName(name),
											A3($author$project$Morphir$IR$Value$Codec$encodeDefinition, encodeTypeAttributes, encodeValueAttributes, def)
										]));
							},
							$elm$core$Dict$toList(valueDefinitions)),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, inValue)
						]));
			case 13:
				var a = v.a;
				var pattern = v.b;
				var valueToDestruct = v.c;
				var inValue = v.d;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('destructure'),
							encodeValueAttributes(a),
							A2($author$project$Morphir$IR$Value$Codec$encodePattern, encodeValueAttributes, pattern),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, valueToDestruct),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, inValue)
						]));
			case 14:
				var a = v.a;
				var condition = v.b;
				var thenBranch = v.c;
				var elseBranch = v.d;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('if_then_else'),
							encodeValueAttributes(a),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, condition),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, thenBranch),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, elseBranch)
						]));
			case 15:
				var a = v.a;
				var branchOutOn = v.b;
				var cases = v.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('pattern_match'),
							encodeValueAttributes(a),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, branchOutOn),
							A2(
							$elm$json$Json$Encode$list,
							function (_v3) {
								var pattern = _v3.a;
								var body = _v3.b;
								return A2(
									$elm$json$Json$Encode$list,
									$elm$core$Basics$identity,
									_List_fromArray(
										[
											A2($author$project$Morphir$IR$Value$Codec$encodePattern, encodeValueAttributes, pattern),
											A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, body)
										]));
							},
							cases)
						]));
			case 16:
				var a = v.a;
				var valueToUpdate = v.b;
				var fieldsToUpdate = v.c;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('update_record'),
							encodeValueAttributes(a),
							A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, valueToUpdate),
							A2(
							$elm$json$Json$Encode$list,
							function (_v4) {
								var fieldName = _v4.a;
								var fieldValue = _v4.b;
								return A2(
									$elm$json$Json$Encode$list,
									$elm$core$Basics$identity,
									_List_fromArray(
										[
											$author$project$Morphir$IR$Name$Codec$encodeName(fieldName),
											A3($author$project$Morphir$IR$Value$Codec$encodeValue, encodeTypeAttributes, encodeValueAttributes, fieldValue)
										]));
							},
							fieldsToUpdate)
						]));
			default:
				var a = v.a;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('unit'),
							encodeValueAttributes(a)
						]));
		}
	});
var $author$project$Morphir$IR$Documented$Codec$encodeDocumented = F2(
	function (encodeValue, d) {
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string(d.en),
					encodeValue(d.aT)
				]));
	});
var $author$project$Morphir$IR$Module$Codec$encodeDefinition = F3(
	function (encodeTypeAttributes, encodeValueAttributes, def) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'types',
					A2(
						$elm$json$Json$Encode$list,
						function (_v0) {
							var name = _v0.a;
							var typeDef = _v0.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$core$Basics$identity,
								_List_fromArray(
									[
										$author$project$Morphir$IR$Name$Codec$encodeName(name),
										A2(
										$author$project$Morphir$IR$AccessControlled$Codec$encodeAccessControlled,
										$author$project$Morphir$IR$Documented$Codec$encodeDocumented(
											$author$project$Morphir$IR$Type$Codec$encodeDefinition(encodeTypeAttributes)),
										typeDef)
									]));
						},
						$elm$core$Dict$toList(def.gn))),
					_Utils_Tuple2(
					'values',
					A2(
						$elm$json$Json$Encode$list,
						function (_v1) {
							var name = _v1.a;
							var valueDef = _v1.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$core$Basics$identity,
								_List_fromArray(
									[
										$author$project$Morphir$IR$Name$Codec$encodeName(name),
										A2(
										$author$project$Morphir$IR$AccessControlled$Codec$encodeAccessControlled,
										A2($author$project$Morphir$IR$Value$Codec$encodeDefinition, encodeTypeAttributes, encodeValueAttributes),
										valueDef)
									]));
						},
						$elm$core$Dict$toList(def.gq)))
				]));
	});
var $author$project$Morphir$IR$Package$Codec$encodeDefinition = F3(
	function (encodeTypeAttributes, encodeValueAttributes, def) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'modules',
					A2(
						$elm$json$Json$Encode$list,
						function (_v0) {
							var moduleName = _v0.a;
							var moduleDef = _v0.b;
							return $elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'name',
										$author$project$Morphir$IR$Path$Codec$encodePath(moduleName)),
										_Utils_Tuple2(
										'def',
										A2(
											$author$project$Morphir$IR$AccessControlled$Codec$encodeAccessControlled,
											A2($author$project$Morphir$IR$Module$Codec$encodeDefinition, encodeTypeAttributes, encodeValueAttributes),
											moduleDef))
									]));
						},
						$elm$core$Dict$toList(def.e7)))
				]));
	});
var $author$project$Morphir$IR$Type$Codec$encodeSpecification = F2(
	function (encodeAttributes, spec) {
		switch (spec.$) {
			case 0:
				var params = spec.a;
				var exp = spec.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('type_alias_specification'),
							A2($elm$json$Json$Encode$list, $author$project$Morphir$IR$Name$Codec$encodeName, params),
							A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, exp)
						]));
			case 1:
				var params = spec.a;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('opaque_type_specification'),
							A2($elm$json$Json$Encode$list, $author$project$Morphir$IR$Name$Codec$encodeName, params)
						]));
			default:
				var params = spec.a;
				var ctors = spec.b;
				return A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$json$Json$Encode$string('custom_type_specification'),
							A2($elm$json$Json$Encode$list, $author$project$Morphir$IR$Name$Codec$encodeName, params),
							A2($author$project$Morphir$IR$Type$Codec$encodeConstructors, encodeAttributes, ctors)
						]));
		}
	});
var $author$project$Morphir$IR$Value$Codec$encodeSpecification = F2(
	function (encodeAttributes, spec) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'inputs',
					A2(
						$elm$json$Json$Encode$list,
						function (_v0) {
							var argName = _v0.a;
							var argType = _v0.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$core$Basics$identity,
								_List_fromArray(
									[
										$author$project$Morphir$IR$Name$Codec$encodeName(argName),
										A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, argType)
									]));
						},
						spec.bo)),
					_Utils_Tuple2(
					'output',
					A2($author$project$Morphir$IR$Type$Codec$encodeType, encodeAttributes, spec.bN))
				]));
	});
var $author$project$Morphir$IR$Module$Codec$encodeSpecification = F2(
	function (encodeTypeAttributes, spec) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'types',
					A2(
						$elm$json$Json$Encode$list,
						function (_v0) {
							var name = _v0.a;
							var typeSpec = _v0.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$core$Basics$identity,
								_List_fromArray(
									[
										$author$project$Morphir$IR$Name$Codec$encodeName(name),
										A2(
										$author$project$Morphir$IR$Documented$Codec$encodeDocumented,
										$author$project$Morphir$IR$Type$Codec$encodeSpecification(encodeTypeAttributes),
										typeSpec)
									]));
						},
						$elm$core$Dict$toList(spec.gn))),
					_Utils_Tuple2(
					'values',
					A2(
						$elm$json$Json$Encode$list,
						function (_v1) {
							var name = _v1.a;
							var valueSpec = _v1.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$core$Basics$identity,
								_List_fromArray(
									[
										$author$project$Morphir$IR$Name$Codec$encodeName(name),
										A2($author$project$Morphir$IR$Value$Codec$encodeSpecification, encodeTypeAttributes, valueSpec)
									]));
						},
						$elm$core$Dict$toList(spec.gq)))
				]));
	});
var $author$project$Morphir$IR$Package$Codec$encodeSpecification = F2(
	function (encodeTypeAttributes, spec) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'modules',
					A2(
						$elm$json$Json$Encode$list,
						function (_v0) {
							var moduleName = _v0.a;
							var moduleSpec = _v0.b;
							return $elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'name',
										$author$project$Morphir$IR$Path$Codec$encodePath(moduleName)),
										_Utils_Tuple2(
										'spec',
										A2($author$project$Morphir$IR$Module$Codec$encodeSpecification, encodeTypeAttributes, moduleSpec))
									]));
						},
						$elm$core$Dict$toList(spec.e7)))
				]));
	});
var $author$project$Morphir$Codec$encodeUnit = function (_v0) {
	return $elm$json$Json$Encode$object(_List_Nil);
};
var $author$project$Morphir$IR$Distribution$Codec$encodeDistribution = function (distro) {
	var packagePath = distro.a;
	var dependencies = distro.b;
	var def = distro.c;
	return A2(
		$elm$json$Json$Encode$list,
		$elm$core$Basics$identity,
		_List_fromArray(
			[
				$elm$json$Json$Encode$string('library'),
				$author$project$Morphir$IR$Path$Codec$encodePath(packagePath),
				A2(
				$elm$json$Json$Encode$list,
				function (_v1) {
					var packageName = _v1.a;
					var packageSpec = _v1.b;
					return A2(
						$elm$json$Json$Encode$list,
						$elm$core$Basics$identity,
						_List_fromArray(
							[
								$author$project$Morphir$IR$Path$Codec$encodePath(packageName),
								A2($author$project$Morphir$IR$Package$Codec$encodeSpecification, $author$project$Morphir$Codec$encodeUnit, packageSpec)
							]));
				},
				$elm$core$Dict$toList(dependencies)),
				A3(
				$author$project$Morphir$IR$Package$Codec$encodeDefinition,
				$author$project$Morphir$Codec$encodeUnit,
				$author$project$Morphir$IR$Type$Codec$encodeType($author$project$Morphir$Codec$encodeUnit),
				def)
			]));
};
var $author$project$Morphir$Compiler$Codec$encodeSourceLocation = function (sourceLocation) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'row',
				$elm$json$Json$Encode$int(sourceLocation.fD)),
				_Utils_Tuple2(
				'column',
				$elm$json$Json$Encode$int(sourceLocation.ec))
			]));
};
var $author$project$Morphir$Compiler$Codec$encodeSourceRange = function (sourceRange) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'start',
				$author$project$Morphir$Compiler$Codec$encodeSourceLocation(sourceRange.fT)),
				_Utils_Tuple2(
				'end',
				$author$project$Morphir$Compiler$Codec$encodeSourceLocation(sourceRange.eq))
			]));
};
var $author$project$Morphir$Compiler$Codec$encodeErrorInSourceFile = function (errorInSourceFile) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'errorMessage',
				$elm$json$Json$Encode$string(errorInSourceFile.er)),
				_Utils_Tuple2(
				'sourceLocations',
				A2($elm$json$Json$Encode$list, $author$project$Morphir$Compiler$Codec$encodeSourceRange, errorInSourceFile.fP))
			]));
};
var $author$project$Morphir$Compiler$Codec$encodeError = function (error) {
	if (!error.$) {
		var filePath = error.a;
		var errorInSourceFiles = error.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string('errors_in_source_file'),
					$elm$json$Json$Encode$string(filePath),
					A2($elm$json$Json$Encode$list, $author$project$Morphir$Compiler$Codec$encodeErrorInSourceFile, errorInSourceFiles)
				]));
	} else {
		var errorAcrossSourceFiles = error.a;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string('error_across_source_files'),
					$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'errorMessage',
							$elm$json$Json$Encode$string(errorAcrossSourceFiles.er)),
							_Utils_Tuple2(
							'files',
							A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, errorAcrossSourceFiles.eC))
						]))
				]));
	}
};
var $author$project$Morphir$File$FileMap$Codec$encodeFileMap = function (fileMap) {
	return A2(
		$elm$json$Json$Encode$list,
		function (_v0) {
			var _v1 = _v0.a;
			var dirPath = _v1.a;
			var fileName = _v1.b;
			var content = _v0.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$json$Json$Encode$list,
						$elm$core$Basics$identity,
						_List_fromArray(
							[
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, dirPath),
								$elm$json$Json$Encode$string(fileName)
							])),
						$elm$json$Json$Encode$string(content)
					]));
		},
		$elm$core$Dict$toList(fileMap));
};
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Morphir$Elm$CLI$encodeResult = F3(
	function (encodeErr, encodeValue, result) {
		if (!result.$) {
			var a = result.a;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$null,
						encodeValue(a)
					]));
		} else {
			var e = result.a;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						encodeErr(e),
						$elm$json$Json$Encode$null
					]));
		}
	});
var $author$project$Morphir$Elm$CLI$generateResult = _Platform_outgoingPort('generateResult', $elm$core$Basics$identity);
var $author$project$Morphir$Compiler$ErrorInSourceFile = F2(
	function (errorMessage, sourceLocations) {
		return {er: errorMessage, fP: sourceLocations};
	});
var $author$project$Morphir$Compiler$ErrorsInSourceFile = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $author$project$Morphir$IR$Value$indexedMapListHelp = F3(
	function (f, baseIndex, elemList) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (nextElem, _v0) {
					var elemsSoFar = _v0.a;
					var lastIndexSoFar = _v0.b;
					var _v1 = A2(f, lastIndexSoFar + 1, nextElem);
					var mappedElem = _v1.a;
					var lastIndex = _v1.b;
					return _Utils_Tuple2(
						A2(
							$elm$core$List$append,
							elemsSoFar,
							_List_fromArray(
								[mappedElem])),
						lastIndex);
				}),
			_Utils_Tuple2(_List_Nil, baseIndex),
			elemList);
	});
var $author$project$Morphir$IR$Value$indexedMapPattern = F3(
	function (f, baseIndex, pattern) {
		switch (pattern.$) {
			case 0:
				var a = pattern.a;
				return _Utils_Tuple2(
					$author$project$Morphir$IR$Value$WildcardPattern(
						A2(f, baseIndex, a)),
					baseIndex);
			case 1:
				var a = pattern.a;
				var aliasedPattern = pattern.b;
				var alias = pattern.c;
				var _v1 = A3($author$project$Morphir$IR$Value$indexedMapPattern, f, baseIndex + 1, aliasedPattern);
				var mappedAliasedPattern = _v1.a;
				var lastIndex = _v1.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$AsPattern,
						A2(f, baseIndex, a),
						mappedAliasedPattern,
						alias),
					lastIndex);
			case 2:
				var a = pattern.a;
				var elemPatterns = pattern.b;
				var _v2 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					$author$project$Morphir$IR$Value$indexedMapPattern(f),
					baseIndex,
					elemPatterns);
				var mappedElemPatterns = _v2.a;
				var elemsLastIndex = _v2.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$TuplePattern,
						A2(f, baseIndex, a),
						mappedElemPatterns),
					elemsLastIndex);
			case 3:
				var a = pattern.a;
				var fQName = pattern.b;
				var argPatterns = pattern.c;
				var _v3 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					$author$project$Morphir$IR$Value$indexedMapPattern(f),
					baseIndex,
					argPatterns);
				var mappedArgPatterns = _v3.a;
				var argPatternsLastIndex = _v3.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$ConstructorPattern,
						A2(f, baseIndex, a),
						fQName,
						mappedArgPatterns),
					argPatternsLastIndex);
			case 4:
				var a = pattern.a;
				return _Utils_Tuple2(
					$author$project$Morphir$IR$Value$EmptyListPattern(
						A2(f, baseIndex, a)),
					baseIndex);
			case 5:
				var a = pattern.a;
				var headPattern = pattern.b;
				var tailPattern = pattern.c;
				var _v4 = A3($author$project$Morphir$IR$Value$indexedMapPattern, f, baseIndex + 1, headPattern);
				var mappedHeadPattern = _v4.a;
				var lastIndexHeadPattern = _v4.b;
				var _v5 = A3($author$project$Morphir$IR$Value$indexedMapPattern, f, lastIndexHeadPattern + 1, tailPattern);
				var mappedTailPattern = _v5.a;
				var lastIndexTailPattern = _v5.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$HeadTailPattern,
						A2(f, baseIndex, a),
						mappedHeadPattern,
						mappedTailPattern),
					lastIndexTailPattern);
			case 6:
				var a = pattern.a;
				var lit = pattern.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$LiteralPattern,
						A2(f, baseIndex, a),
						lit),
					baseIndex);
			default:
				var a = pattern.a;
				return _Utils_Tuple2(
					$author$project$Morphir$IR$Value$UnitPattern(
						A2(f, baseIndex, a)),
					baseIndex);
		}
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === -2) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Morphir$IR$Value$indexedMapValue = F3(
	function (f, baseIndex, value) {
		switch (value.$) {
			case 0:
				var a = value.a;
				var lit = value.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$Literal,
						A2(f, baseIndex, a),
						lit),
					baseIndex);
			case 1:
				var a = value.a;
				var fullyQualifiedName = value.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$Constructor,
						A2(f, baseIndex, a),
						fullyQualifiedName),
					baseIndex);
			case 2:
				var a = value.a;
				var elems = value.b;
				var _v1 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					$author$project$Morphir$IR$Value$indexedMapValue(f),
					baseIndex,
					elems);
				var mappedElems = _v1.a;
				var elemsLastIndex = _v1.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$Tuple,
						A2(f, baseIndex, a),
						mappedElems),
					elemsLastIndex);
			case 3:
				var a = value.a;
				var values = value.b;
				var _v2 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					$author$project$Morphir$IR$Value$indexedMapValue(f),
					baseIndex,
					values);
				var mappedValues = _v2.a;
				var valuesLastIndex = _v2.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$List,
						A2(f, baseIndex, a),
						mappedValues),
					valuesLastIndex);
			case 4:
				var a = value.a;
				var fields = value.b;
				var _v3 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					F2(
						function (fieldBaseIndex, _v4) {
							var fieldName = _v4.a;
							var fieldValue = _v4.b;
							var _v5 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, fieldBaseIndex, fieldValue);
							var mappedFieldValue = _v5.a;
							var lastFieldIndex = _v5.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(fieldName, mappedFieldValue),
								lastFieldIndex);
						}),
					baseIndex,
					fields);
				var mappedFields = _v3.a;
				var valuesLastIndex = _v3.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$Record,
						A2(f, baseIndex, a),
						mappedFields),
					valuesLastIndex);
			case 5:
				var a = value.a;
				var name = value.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$Variable,
						A2(f, baseIndex, a),
						name),
					baseIndex);
			case 6:
				var a = value.a;
				var fQName = value.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$Reference,
						A2(f, baseIndex, a),
						fQName),
					baseIndex);
			case 7:
				var a = value.a;
				var subjectValue = value.b;
				var name = value.c;
				var _v6 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, baseIndex + 1, subjectValue);
				var mappedSubjectValue = _v6.a;
				var subjectValueLastIndex = _v6.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$Field,
						A2(f, baseIndex, a),
						mappedSubjectValue,
						name),
					subjectValueLastIndex);
			case 8:
				var a = value.a;
				var name = value.b;
				return _Utils_Tuple2(
					A2(
						$author$project$Morphir$IR$Value$FieldFunction,
						A2(f, baseIndex, a),
						name),
					baseIndex);
			case 9:
				var a = value.a;
				var funValue = value.b;
				var argValue = value.c;
				var _v7 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, baseIndex + 1, funValue);
				var mappedFunValue = _v7.a;
				var funValueLastIndex = _v7.b;
				var _v8 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, funValueLastIndex + 1, argValue);
				var mappedArgValue = _v8.a;
				var argValueLastIndex = _v8.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$Apply,
						A2(f, baseIndex, a),
						mappedFunValue,
						mappedArgValue),
					argValueLastIndex);
			case 10:
				var a = value.a;
				var argPattern = value.b;
				var bodyValue = value.c;
				var _v9 = A3($author$project$Morphir$IR$Value$indexedMapPattern, f, baseIndex + 1, argPattern);
				var mappedArgPattern = _v9.a;
				var argPatternLastIndex = _v9.b;
				var _v10 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, argPatternLastIndex + 1, bodyValue);
				var mappedBodyValue = _v10.a;
				var bodyValueLastIndex = _v10.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$Lambda,
						A2(f, baseIndex, a),
						mappedArgPattern,
						mappedBodyValue),
					bodyValueLastIndex);
			case 11:
				var a = value.a;
				var defName = value.b;
				var def = value.c;
				var inValue = value.d;
				var _v11 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					F2(
						function (inputBaseIndex, _v12) {
							var inputName = _v12.a;
							var inputA = _v12.b;
							var inputType = _v12.c;
							return _Utils_Tuple2(
								_Utils_Tuple3(
									inputName,
									A2(f, inputBaseIndex, inputA),
									inputType),
								inputBaseIndex);
						}),
					baseIndex,
					def.a3);
				var mappedDefArgs = _v11.a;
				var defArgsLastIndex = _v11.b;
				var _v13 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, defArgsLastIndex + 1, def.dV);
				var mappedDefBody = _v13.a;
				var defBodyLastIndex = _v13.b;
				var _v14 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, defBodyLastIndex + 1, inValue);
				var mappedInValue = _v14.a;
				var inValueLastIndex = _v14.b;
				var mappedDef = {dV: mappedDefBody, a3: mappedDefArgs, fq: def.fq};
				return _Utils_Tuple2(
					A4(
						$author$project$Morphir$IR$Value$LetDefinition,
						A2(f, baseIndex, a),
						defName,
						mappedDef,
						mappedInValue),
					inValueLastIndex);
			case 12:
				var a = value.a;
				var defs = value.b;
				var inValue = value.c;
				var _v15 = $elm$core$Dict$isEmpty(defs) ? _Utils_Tuple2(_List_Nil, baseIndex) : A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					F2(
						function (defBaseIndex, _v16) {
							var defName = _v16.a;
							var def = _v16.b;
							var _v17 = A3(
								$author$project$Morphir$IR$Value$indexedMapListHelp,
								F2(
									function (inputBaseIndex, _v18) {
										var inputName = _v18.a;
										var inputA = _v18.b;
										var inputType = _v18.c;
										return _Utils_Tuple2(
											_Utils_Tuple3(
												inputName,
												A2(f, inputBaseIndex, inputA),
												inputType),
											inputBaseIndex);
									}),
								defBaseIndex - 1,
								def.a3);
							var mappedDefArgs = _v17.a;
							var defArgsLastIndex = _v17.b;
							var _v19 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, defArgsLastIndex + 1, def.dV);
							var mappedDefBody = _v19.a;
							var defBodyLastIndex = _v19.b;
							var mappedDef = {dV: mappedDefBody, a3: mappedDefArgs, fq: def.fq};
							return _Utils_Tuple2(
								_Utils_Tuple2(defName, mappedDef),
								defBodyLastIndex);
						}),
					baseIndex,
					$elm$core$Dict$toList(defs));
				var mappedDefs = _v15.a;
				var defsLastIndex = _v15.b;
				var _v20 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, defsLastIndex + 1, inValue);
				var mappedInValue = _v20.a;
				var inValueLastIndex = _v20.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$LetRecursion,
						A2(f, baseIndex, a),
						$elm$core$Dict$fromList(mappedDefs),
						mappedInValue),
					inValueLastIndex);
			case 13:
				var a = value.a;
				var bindPattern = value.b;
				var bindValue = value.c;
				var inValue = value.d;
				var _v21 = A3($author$project$Morphir$IR$Value$indexedMapPattern, f, baseIndex + 1, bindPattern);
				var mappedBindPattern = _v21.a;
				var bindPatternLastIndex = _v21.b;
				var _v22 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, bindPatternLastIndex + 1, bindValue);
				var mappedBindValue = _v22.a;
				var bindValueLastIndex = _v22.b;
				var _v23 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, bindValueLastIndex + 1, inValue);
				var mappedInValue = _v23.a;
				var inValueLastIndex = _v23.b;
				return _Utils_Tuple2(
					A4(
						$author$project$Morphir$IR$Value$Destructure,
						A2(f, baseIndex, a),
						mappedBindPattern,
						mappedBindValue,
						mappedInValue),
					inValueLastIndex);
			case 14:
				var a = value.a;
				var condValue = value.b;
				var thenValue = value.c;
				var elseValue = value.d;
				var _v24 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, baseIndex + 1, condValue);
				var mappedCondValue = _v24.a;
				var condValueLastIndex = _v24.b;
				var _v25 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, condValueLastIndex + 1, thenValue);
				var mappedThenValue = _v25.a;
				var thenValueLastIndex = _v25.b;
				var _v26 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, thenValueLastIndex + 1, elseValue);
				var mappedElseValue = _v26.a;
				var elseValueLastIndex = _v26.b;
				return _Utils_Tuple2(
					A4(
						$author$project$Morphir$IR$Value$IfThenElse,
						A2(f, baseIndex, a),
						mappedCondValue,
						mappedThenValue,
						mappedElseValue),
					elseValueLastIndex);
			case 15:
				var a = value.a;
				var subjectValue = value.b;
				var cases = value.c;
				var _v27 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, baseIndex + 1, subjectValue);
				var mappedSubjectValue = _v27.a;
				var subjectValueLastIndex = _v27.b;
				var _v28 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					F2(
						function (fieldBaseIndex, _v29) {
							var casePattern = _v29.a;
							var caseBody = _v29.b;
							var _v30 = A3($author$project$Morphir$IR$Value$indexedMapPattern, f, fieldBaseIndex, casePattern);
							var mappedCasePattern = _v30.a;
							var casePatternLastIndex = _v30.b;
							var _v31 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, casePatternLastIndex + 1, caseBody);
							var mappedCaseBody = _v31.a;
							var caseBodyLastIndex = _v31.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(mappedCasePattern, mappedCaseBody),
								caseBodyLastIndex);
						}),
					subjectValueLastIndex + 1,
					cases);
				var mappedCases = _v28.a;
				var casesLastIndex = _v28.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$PatternMatch,
						A2(f, baseIndex, a),
						mappedSubjectValue,
						mappedCases),
					casesLastIndex);
			case 16:
				var a = value.a;
				var subjectValue = value.b;
				var fields = value.c;
				var _v32 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, baseIndex + 1, subjectValue);
				var mappedSubjectValue = _v32.a;
				var subjectValueLastIndex = _v32.b;
				var _v33 = A3(
					$author$project$Morphir$IR$Value$indexedMapListHelp,
					F2(
						function (fieldBaseIndex, _v34) {
							var fieldName = _v34.a;
							var fieldValue = _v34.b;
							var _v35 = A3($author$project$Morphir$IR$Value$indexedMapValue, f, fieldBaseIndex, fieldValue);
							var mappedFieldValue = _v35.a;
							var lastFieldIndex = _v35.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(fieldName, mappedFieldValue),
								lastFieldIndex);
						}),
					subjectValueLastIndex + 1,
					fields);
				var mappedFields = _v33.a;
				var valuesLastIndex = _v33.b;
				return _Utils_Tuple2(
					A3(
						$author$project$Morphir$IR$Value$UpdateRecord,
						A2(f, baseIndex, a),
						mappedSubjectValue,
						mappedFields),
					valuesLastIndex);
			default:
				var a = value.a;
				return _Utils_Tuple2(
					$author$project$Morphir$IR$Value$Unit(
						A2(f, baseIndex, a)),
					baseIndex);
		}
	});
var $author$project$Morphir$Type$MetaType$variable = function (i) {
	return _Utils_Tuple2(i, 0);
};
var $author$project$Morphir$Type$Infer$annotateValue = F2(
	function (baseIndex, untypedValue) {
		return A3(
			$author$project$Morphir$IR$Value$indexedMapValue,
			F2(
				function (index, va) {
					return _Utils_Tuple2(
						va,
						$author$project$Morphir$Type$MetaType$variable(index));
				}),
			baseIndex,
			untypedValue);
	});
var $author$project$Morphir$Type$Infer$annotateDefinition = F2(
	function (baseIndex, def) {
		var annotatedInputTypes = A2(
			$elm$core$List$indexedMap,
			F2(
				function (index, _v1) {
					var name = _v1.a;
					var va = _v1.b;
					var tpe = _v1.c;
					return _Utils_Tuple3(
						name,
						_Utils_Tuple2(
							va,
							$author$project$Morphir$Type$MetaType$variable(baseIndex + index)),
						tpe);
				}),
			def.a3);
		var _v0 = A2(
			$author$project$Morphir$Type$Infer$annotateValue,
			baseIndex + $elm$core$List$length(def.a3),
			def.dV);
		var annotatedBody = _v0.a;
		var lastVarIndex = _v0.b;
		return _Utils_Tuple2(
			{dV: annotatedBody, a3: annotatedInputTypes, fq: def.fq},
			lastVarIndex);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $author$project$Morphir$Type$SolutionMap$get = F2(
	function (_var, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$get, _var, dict);
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$Morphir$IR$Value$mapPatternAttributes = F2(
	function (f, p) {
		switch (p.$) {
			case 0:
				var a = p.a;
				return $author$project$Morphir$IR$Value$WildcardPattern(
					f(a));
			case 1:
				var a = p.a;
				var p2 = p.b;
				var name = p.c;
				return A3(
					$author$project$Morphir$IR$Value$AsPattern,
					f(a),
					A2($author$project$Morphir$IR$Value$mapPatternAttributes, f, p2),
					name);
			case 2:
				var a = p.a;
				var elementPatterns = p.b;
				return A2(
					$author$project$Morphir$IR$Value$TuplePattern,
					f(a),
					A2(
						$elm$core$List$map,
						$author$project$Morphir$IR$Value$mapPatternAttributes(f),
						elementPatterns));
			case 3:
				var a = p.a;
				var constructorName = p.b;
				var argumentPatterns = p.c;
				return A3(
					$author$project$Morphir$IR$Value$ConstructorPattern,
					f(a),
					constructorName,
					A2(
						$elm$core$List$map,
						$author$project$Morphir$IR$Value$mapPatternAttributes(f),
						argumentPatterns));
			case 4:
				var a = p.a;
				return $author$project$Morphir$IR$Value$EmptyListPattern(
					f(a));
			case 5:
				var a = p.a;
				var headPattern = p.b;
				var tailPattern = p.c;
				return A3(
					$author$project$Morphir$IR$Value$HeadTailPattern,
					f(a),
					A2($author$project$Morphir$IR$Value$mapPatternAttributes, f, headPattern),
					A2($author$project$Morphir$IR$Value$mapPatternAttributes, f, tailPattern));
			case 6:
				var a = p.a;
				var value = p.b;
				return A2(
					$author$project$Morphir$IR$Value$LiteralPattern,
					f(a),
					value);
			default:
				var a = p.a;
				return $author$project$Morphir$IR$Value$UnitPattern(
					f(a));
		}
	});
var $author$project$Morphir$IR$Type$mapFieldType = F2(
	function (f, field) {
		return A2(
			$author$project$Morphir$IR$Type$Field,
			field.cN,
			f(field.gl));
	});
var $author$project$Morphir$IR$Type$mapTypeAttributes = F2(
	function (f, tpe) {
		switch (tpe.$) {
			case 0:
				var a = tpe.a;
				var name = tpe.b;
				return A2(
					$author$project$Morphir$IR$Type$Variable,
					f(a),
					name);
			case 1:
				var a = tpe.a;
				var fQName = tpe.b;
				var argTypes = tpe.c;
				return A3(
					$author$project$Morphir$IR$Type$Reference,
					f(a),
					fQName,
					A2(
						$elm$core$List$map,
						$author$project$Morphir$IR$Type$mapTypeAttributes(f),
						argTypes));
			case 2:
				var a = tpe.a;
				var elemTypes = tpe.b;
				return A2(
					$author$project$Morphir$IR$Type$Tuple,
					f(a),
					A2(
						$elm$core$List$map,
						$author$project$Morphir$IR$Type$mapTypeAttributes(f),
						elemTypes));
			case 3:
				var a = tpe.a;
				var fields = tpe.b;
				return A2(
					$author$project$Morphir$IR$Type$Record,
					f(a),
					A2(
						$elm$core$List$map,
						$author$project$Morphir$IR$Type$mapFieldType(
							$author$project$Morphir$IR$Type$mapTypeAttributes(f)),
						fields));
			case 4:
				var a = tpe.a;
				var name = tpe.b;
				var fields = tpe.c;
				return A3(
					$author$project$Morphir$IR$Type$ExtensibleRecord,
					f(a),
					name,
					A2(
						$elm$core$List$map,
						$author$project$Morphir$IR$Type$mapFieldType(
							$author$project$Morphir$IR$Type$mapTypeAttributes(f)),
						fields));
			case 5:
				var a = tpe.a;
				var argType = tpe.b;
				var returnType = tpe.c;
				return A3(
					$author$project$Morphir$IR$Type$Function,
					f(a),
					A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, argType),
					A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, returnType));
			default:
				var a = tpe.a;
				return $author$project$Morphir$IR$Type$Unit(
					f(a));
		}
	});
var $author$project$Morphir$IR$Value$mapDefinitionAttributes = F3(
	function (f, g, d) {
		return A3(
			$author$project$Morphir$IR$Value$Definition,
			A2(
				$elm$core$List$map,
				function (_v5) {
					var name = _v5.a;
					var attr = _v5.b;
					var tpe = _v5.c;
					return _Utils_Tuple3(
						name,
						g(attr),
						A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, tpe));
				},
				d.a3),
			A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, d.fq),
			A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, d.dV));
	});
var $author$project$Morphir$IR$Value$mapValueAttributes = F3(
	function (f, g, v) {
		switch (v.$) {
			case 0:
				var a = v.a;
				var value = v.b;
				return A2(
					$author$project$Morphir$IR$Value$Literal,
					g(a),
					value);
			case 1:
				var a = v.a;
				var fullyQualifiedName = v.b;
				return A2(
					$author$project$Morphir$IR$Value$Constructor,
					g(a),
					fullyQualifiedName);
			case 2:
				var a = v.a;
				var elements = v.b;
				return A2(
					$author$project$Morphir$IR$Value$Tuple,
					g(a),
					A2(
						$elm$core$List$map,
						A2($author$project$Morphir$IR$Value$mapValueAttributes, f, g),
						elements));
			case 3:
				var a = v.a;
				var items = v.b;
				return A2(
					$author$project$Morphir$IR$Value$List,
					g(a),
					A2(
						$elm$core$List$map,
						A2($author$project$Morphir$IR$Value$mapValueAttributes, f, g),
						items));
			case 4:
				var a = v.a;
				var fields = v.b;
				return A2(
					$author$project$Morphir$IR$Value$Record,
					g(a),
					A2(
						$elm$core$List$map,
						function (_v1) {
							var fieldName = _v1.a;
							var fieldValue = _v1.b;
							return _Utils_Tuple2(
								fieldName,
								A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, fieldValue));
						},
						fields));
			case 5:
				var a = v.a;
				var name = v.b;
				return A2(
					$author$project$Morphir$IR$Value$Variable,
					g(a),
					name);
			case 6:
				var a = v.a;
				var fullyQualifiedName = v.b;
				return A2(
					$author$project$Morphir$IR$Value$Reference,
					g(a),
					fullyQualifiedName);
			case 7:
				var a = v.a;
				var subjectValue = v.b;
				var fieldName = v.c;
				return A3(
					$author$project$Morphir$IR$Value$Field,
					g(a),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, subjectValue),
					fieldName);
			case 8:
				var a = v.a;
				var fieldName = v.b;
				return A2(
					$author$project$Morphir$IR$Value$FieldFunction,
					g(a),
					fieldName);
			case 9:
				var a = v.a;
				var _function = v.b;
				var argument = v.c;
				return A3(
					$author$project$Morphir$IR$Value$Apply,
					g(a),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, _function),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, argument));
			case 10:
				var a = v.a;
				var argumentPattern = v.b;
				var body = v.c;
				return A3(
					$author$project$Morphir$IR$Value$Lambda,
					g(a),
					A2($author$project$Morphir$IR$Value$mapPatternAttributes, g, argumentPattern),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, body));
			case 11:
				var a = v.a;
				var valueName = v.b;
				var valueDefinition = v.c;
				var inValue = v.d;
				return A4(
					$author$project$Morphir$IR$Value$LetDefinition,
					g(a),
					valueName,
					A3($author$project$Morphir$IR$Value$mapDefinitionAttributes, f, g, valueDefinition),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, inValue));
			case 12:
				var a = v.a;
				var valueDefinitions = v.b;
				var inValue = v.c;
				return A3(
					$author$project$Morphir$IR$Value$LetRecursion,
					g(a),
					A2(
						$elm$core$Dict$map,
						F2(
							function (_v2, def) {
								return A3($author$project$Morphir$IR$Value$mapDefinitionAttributes, f, g, def);
							}),
						valueDefinitions),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, inValue));
			case 13:
				var a = v.a;
				var pattern = v.b;
				var valueToDestruct = v.c;
				var inValue = v.d;
				return A4(
					$author$project$Morphir$IR$Value$Destructure,
					g(a),
					A2($author$project$Morphir$IR$Value$mapPatternAttributes, g, pattern),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, valueToDestruct),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, inValue));
			case 14:
				var a = v.a;
				var condition = v.b;
				var thenBranch = v.c;
				var elseBranch = v.d;
				return A4(
					$author$project$Morphir$IR$Value$IfThenElse,
					g(a),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, condition),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, thenBranch),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, elseBranch));
			case 15:
				var a = v.a;
				var branchOutOn = v.b;
				var cases = v.c;
				return A3(
					$author$project$Morphir$IR$Value$PatternMatch,
					g(a),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, branchOutOn),
					A2(
						$elm$core$List$map,
						function (_v3) {
							var pattern = _v3.a;
							var body = _v3.b;
							return _Utils_Tuple2(
								A2($author$project$Morphir$IR$Value$mapPatternAttributes, g, pattern),
								A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, body));
						},
						cases));
			case 16:
				var a = v.a;
				var valueToUpdate = v.b;
				var fieldsToUpdate = v.c;
				return A3(
					$author$project$Morphir$IR$Value$UpdateRecord,
					g(a),
					A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, valueToUpdate),
					A2(
						$elm$core$List$map,
						function (_v4) {
							var fieldName = _v4.a;
							var fieldValue = _v4.b;
							return _Utils_Tuple2(
								fieldName,
								A3($author$project$Morphir$IR$Value$mapValueAttributes, f, g, fieldValue));
						},
						fieldsToUpdate));
			default:
				var a = v.a;
				return $author$project$Morphir$IR$Value$Unit(
					g(a));
		}
	});
var $author$project$Morphir$Type$MetaType$toName = function (_v0) {
	var i = _v0.a;
	var s = _v0.b;
	return _List_fromArray(
		[
			't',
			$elm$core$String$fromInt(i),
			$elm$core$String$fromInt(s)
		]);
};
var $author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType = F2(
	function (solutionMap, metaType) {
		metaTypeToConcreteType:
		while (true) {
			switch (metaType.$) {
				case 0:
					var metaVar = metaType.a;
					return A2(
						$elm$core$Maybe$withDefault,
						A2(
							$author$project$Morphir$IR$Type$Variable,
							0,
							$author$project$Morphir$Type$MetaType$toName(metaVar)),
						A2(
							$elm$core$Maybe$map,
							$author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType(solutionMap),
							A2($author$project$Morphir$Type$SolutionMap$get, metaVar, solutionMap)));
				case 2:
					var metaElems = metaType.a;
					return A2(
						$author$project$Morphir$IR$Type$Tuple,
						0,
						A2(
							$elm$core$List$map,
							$author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType(solutionMap),
							metaElems));
				case 3:
					var _extends = metaType.a;
					var metaFields = metaType.b;
					if (_extends.$ === 1) {
						return A2(
							$author$project$Morphir$IR$Type$Record,
							0,
							A2(
								$elm$core$List$map,
								function (_v2) {
									var fieldName = _v2.a;
									var fieldType = _v2.b;
									return A2(
										$author$project$Morphir$IR$Type$Field,
										fieldName,
										A2($author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType, solutionMap, fieldType));
								},
								$elm$core$Dict$toList(metaFields)));
					} else {
						var baseType = _extends.a;
						return A3(
							$author$project$Morphir$IR$Type$ExtensibleRecord,
							0,
							$author$project$Morphir$Type$MetaType$toName(baseType),
							A2(
								$elm$core$List$map,
								function (_v3) {
									var fieldName = _v3.a;
									var fieldType = _v3.b;
									return A2(
										$author$project$Morphir$IR$Type$Field,
										fieldName,
										A2($author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType, solutionMap, fieldType));
								},
								$elm$core$Dict$toList(metaFields)));
					}
				case 4:
					var uncurry = function (mt) {
						if (mt.$ === 4) {
							var mf = mt.a;
							var ma = mt.b;
							var _v5 = uncurry(mf);
							var f = _v5.a;
							var args = _v5.b;
							return _Utils_Tuple2(
								f,
								_Utils_ap(
									args,
									_List_fromArray(
										[ma])));
						} else {
							return _Utils_Tuple2(mt, _List_Nil);
						}
					};
					var _v6 = uncurry(metaType);
					var metaFun = _v6.a;
					var metaArgs = _v6.b;
					switch (metaFun.$) {
						case 1:
							var fQName = metaFun.a;
							return A3(
								$author$project$Morphir$IR$Type$Reference,
								0,
								fQName,
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType(solutionMap),
									metaArgs));
						case 7:
							var alias = metaFun.a;
							return A3(
								$author$project$Morphir$IR$Type$Reference,
								0,
								alias,
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType(solutionMap),
									metaArgs));
						default:
							var other = metaFun;
							var $temp$solutionMap = solutionMap,
								$temp$metaType = other;
							solutionMap = $temp$solutionMap;
							metaType = $temp$metaType;
							continue metaTypeToConcreteType;
					}
				case 5:
					var argType = metaType.a;
					var returnType = metaType.b;
					return A3(
						$author$project$Morphir$IR$Type$Function,
						0,
						A2($author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType, solutionMap, argType),
						A2($author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType, solutionMap, returnType));
				case 1:
					var fQName = metaType.a;
					return A3($author$project$Morphir$IR$Type$Reference, 0, fQName, _List_Nil);
				case 6:
					return $author$project$Morphir$IR$Type$Unit(0);
				default:
					var alias = metaType.a;
					return A3($author$project$Morphir$IR$Type$Reference, 0, alias, _List_Nil);
			}
		}
	});
var $author$project$Morphir$Type$Infer$applySolutionToAnnotatedDefinition = F2(
	function (annotatedDef, _v0) {
		var residualConstraints = _v0.a;
		var solutionMap = _v0.b;
		return A3(
			$author$project$Morphir$IR$Value$mapDefinitionAttributes,
			$elm$core$Basics$identity,
			function (_v1) {
				var va = _v1.a;
				var metaVar = _v1.b;
				return _Utils_Tuple2(
					va,
					A2(
						$elm$core$Maybe$withDefault,
						A2(
							$author$project$Morphir$IR$Type$Variable,
							0,
							$author$project$Morphir$Type$MetaType$toName(metaVar)),
						A2(
							$elm$core$Maybe$map,
							$author$project$Morphir$Type$MetaTypeMapping$metaTypeToConcreteType(solutionMap),
							A2($author$project$Morphir$Type$SolutionMap$get, metaVar, solutionMap))));
			},
			annotatedDef);
	});
var $author$project$Morphir$Type$MetaType$MetaFun = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Morphir$Type$MetaType$MetaRecord = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Morphir$Type$MetaType$MetaTuple = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Type$MetaType$MetaUnit = {$: 6};
var $author$project$Morphir$Type$MetaType$MetaVar = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Type$MetaType$MetaRef = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$IR$FQName$fQName = F3(
	function (packagePath, modulePath, localName) {
		return A3($author$project$Morphir$IR$FQName$FQName, packagePath, modulePath, localName);
	});
var $author$project$Morphir$IR$FQName$fqn = F3(
	function (packageName, moduleName, localName) {
		return A3(
			$author$project$Morphir$IR$FQName$fQName,
			$author$project$Morphir$IR$Path$fromString(packageName),
			$author$project$Morphir$IR$Path$fromString(moduleName),
			$author$project$Morphir$IR$Name$fromString(localName));
	});
var $author$project$Morphir$Type$MetaType$boolType = $author$project$Morphir$Type$MetaType$MetaRef(
	A3($author$project$Morphir$IR$FQName$fqn, 'Morphir.SDK', 'Basics', 'Bool'));
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $elm$core$Set$singleton = function (key) {
	return A2($elm$core$Dict$singleton, key, 0);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$union, dict1, dict2);
	});
var $author$project$Morphir$IR$Type$collectVariables = function (tpe) {
	var collectUnion = function (values) {
		return A3(
			$elm$core$List$foldl,
			$elm$core$Set$union,
			$elm$core$Set$empty,
			A2($elm$core$List$map, $author$project$Morphir$IR$Type$collectVariables, values));
	};
	switch (tpe.$) {
		case 0:
			var name = tpe.b;
			return $elm$core$Set$singleton(name);
		case 1:
			var args = tpe.c;
			return collectUnion(args);
		case 2:
			var elements = tpe.b;
			return collectUnion(elements);
		case 3:
			var fields = tpe.b;
			return collectUnion(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.gl;
					},
					fields));
		case 4:
			var subjectName = tpe.b;
			var fields = tpe.c;
			return A2(
				$elm$core$Set$insert,
				subjectName,
				collectUnion(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.gl;
						},
						fields)));
		case 5:
			var argType = tpe.b;
			var returnType = tpe.c;
			return collectUnion(
				_List_fromArray(
					[argType, returnType]));
		default:
			return $elm$core$Set$empty;
	}
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Morphir$Type$ConstraintSet$ConstraintSet = $elm$core$Basics$identity;
var $author$project$Morphir$Type$ConstraintSet$empty = _List_Nil;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $author$project$Morphir$Type$Constraint$equivalent = F2(
	function (constraint1, constraint2) {
		if (_Utils_eq(constraint1, constraint2)) {
			return true;
		} else {
			var _v0 = _Utils_Tuple2(constraint1, constraint2);
			if ((!_v0.a.$) && (!_v0.b.$)) {
				var _v1 = _v0.a;
				var a1 = _v1.a;
				var a2 = _v1.b;
				var _v2 = _v0.b;
				var b1 = _v2.a;
				var b2 = _v2.b;
				return (_Utils_eq(a1, b1) && _Utils_eq(a2, b2)) || (_Utils_eq(a1, b2) && _Utils_eq(a2, b1));
			} else {
				return false;
			}
		}
	});
var $author$project$Morphir$Type$ConstraintSet$member = F2(
	function (constraint, _v0) {
		var constraints = _v0;
		return A2(
			$elm$core$List$any,
			$author$project$Morphir$Type$Constraint$equivalent(constraint),
			constraints);
	});
var $author$project$Morphir$Type$ConstraintSet$insert = F2(
	function (constraint, constraintSet) {
		var constraints = constraintSet;
		return A2($author$project$Morphir$Type$ConstraintSet$member, constraint, constraintSet) ? constraintSet : A2($elm$core$List$cons, constraint, constraints);
	});
var $author$project$Morphir$Type$ConstraintSet$union = F2(
	function (constraintSet1, _v0) {
		var constraints2 = _v0;
		return A3($elm$core$List$foldl, $author$project$Morphir$Type$ConstraintSet$insert, constraintSet1, constraints2);
	});
var $author$project$Morphir$Type$ConstraintSet$concat = function (constraintSets) {
	return A3($elm$core$List$foldl, $author$project$Morphir$Type$ConstraintSet$union, $author$project$Morphir$Type$ConstraintSet$empty, constraintSets);
};
var $author$project$Morphir$Type$MetaType$MetaAlias = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $author$project$Morphir$Type$MetaType$MetaApply = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Morphir$Type$MetaTypeMapping$CouldNotFindAlias = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Type$MetaTypeMapping$ExpectedAlias = function (a) {
	return {$: 3, a: a};
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (!maybe.$) {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $author$project$Morphir$Type$MetaTypeMapping$lookupAliasedType = F3(
	function (baseVar, refs, fQName) {
		var packageName = fQName.a;
		var moduleName = fQName.b;
		var localName = fQName.c;
		return A2(
			$elm$core$Result$andThen,
			function (typeSpec) {
				var _v0 = typeSpec.aT;
				if (!_v0.$) {
					var paramNames = _v0.a;
					var tpe = _v0.b;
					return $elm$core$Result$Ok(tpe);
				} else {
					return $elm$core$Result$Err(
						$author$project$Morphir$Type$MetaTypeMapping$ExpectedAlias(fQName));
				}
			},
			A2(
				$elm$core$Result$fromMaybe,
				$author$project$Morphir$Type$MetaTypeMapping$CouldNotFindAlias(fQName),
				A2(
					$elm$core$Maybe$andThen,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.gn;
						},
						$elm$core$Dict$get(localName)),
					A2(
						$elm$core$Maybe$andThen,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.e7;
							},
							$elm$core$Dict$get(moduleName)),
						A2($elm$core$Dict$get, packageName, refs)))));
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType = F4(
	function (baseVar, refs, varToMeta, tpe) {
		switch (tpe.$) {
			case 0:
				var varName = tpe.b;
				return $author$project$Morphir$Type$MetaType$MetaVar(
					A2(
						$elm$core$Maybe$withDefault,
						baseVar,
						A2($elm$core$Dict$get, varName, varToMeta)));
			case 1:
				var fQName = tpe.b;
				var args = tpe.c;
				var resolveAliases = function (fqn) {
					return A2(
						$elm$core$Result$withDefault,
						$author$project$Morphir$Type$MetaType$MetaRef(fqn),
						A2(
							$elm$core$Result$map,
							A2(
								$elm$core$Basics$composeR,
								A3($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta),
								$author$project$Morphir$Type$MetaType$MetaAlias(fqn)),
							A3($author$project$Morphir$Type$MetaTypeMapping$lookupAliasedType, baseVar, refs, fqn)));
				};
				var curry = function (argsReversed) {
					if (!argsReversed.b) {
						return resolveAliases(fQName);
					} else {
						var lastArg = argsReversed.a;
						var initArgsReversed = argsReversed.b;
						return A2(
							$author$project$Morphir$Type$MetaType$MetaApply,
							curry(initArgsReversed),
							A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta, lastArg));
					}
				};
				return curry(
					$elm$core$List$reverse(args));
			case 2:
				var elemTypes = tpe.b;
				return $author$project$Morphir$Type$MetaType$MetaTuple(
					A2(
						$elm$core$List$map,
						A3($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta),
						elemTypes));
			case 3:
				var fieldTypes = tpe.b;
				return A2(
					$author$project$Morphir$Type$MetaType$MetaRecord,
					$elm$core$Maybe$Nothing,
					$elm$core$Dict$fromList(
						A2(
							$elm$core$List$map,
							function (field) {
								return _Utils_Tuple2(
									field.cN,
									A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta, field.gl));
							},
							fieldTypes)));
			case 4:
				var subjectName = tpe.b;
				var fieldTypes = tpe.c;
				return A2(
					$author$project$Morphir$Type$MetaType$MetaRecord,
					A2($elm$core$Dict$get, subjectName, varToMeta),
					$elm$core$Dict$fromList(
						A2(
							$elm$core$List$map,
							function (field) {
								return _Utils_Tuple2(
									field.cN,
									A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta, field.gl));
							},
							fieldTypes)));
			case 5:
				var argType = tpe.b;
				var returnType = tpe.c;
				return A2(
					$author$project$Morphir$Type$MetaType$MetaFun,
					A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta, argType),
					A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta, returnType));
			default:
				return $author$project$Morphir$Type$MetaType$MetaUnit;
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Morphir$Type$MetaType$subVariable = function (_v0) {
	var i = _v0.a;
	var s = _v0.b;
	return _Utils_Tuple2(i, s + 1);
};
var $author$project$Morphir$Type$MetaTypeMapping$concreteVarsToMetaVars = F2(
	function (baseVar, variables) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (varName, _v0) {
					var metaVarSoFar = _v0.a;
					var varToMetaSoFar = _v0.b;
					var nextVar = $author$project$Morphir$Type$MetaType$subVariable(metaVarSoFar);
					return _Utils_Tuple2(
						nextVar,
						A3($elm$core$Dict$insert, varName, nextVar, varToMetaSoFar));
				}),
			_Utils_Tuple2(baseVar, $elm$core$Dict$empty),
			$elm$core$Set$toList(variables)).b;
	});
var $author$project$Morphir$Type$Class$Number = 0;
var $author$project$Morphir$Type$MetaType$charType = $author$project$Morphir$Type$MetaType$MetaRef(
	A3($author$project$Morphir$IR$FQName$fqn, 'Morphir.SDK', 'Char', 'Char'));
var $author$project$Morphir$Type$Constraint$Class = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Morphir$Type$Constraint$class = $author$project$Morphir$Type$Constraint$Class;
var $author$project$Morphir$Type$Constraint$Equality = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$Type$Constraint$equality = $author$project$Morphir$Type$Constraint$Equality;
var $author$project$Morphir$Type$MetaType$floatType = $author$project$Morphir$Type$MetaType$MetaRef(
	A3($author$project$Morphir$IR$FQName$fqn, 'Morphir.SDK', 'Basics', 'Float'));
var $author$project$Morphir$Type$ConstraintSet$singleton = function (constraint) {
	return _List_fromArray(
		[constraint]);
};
var $author$project$Morphir$Type$MetaType$stringType = $author$project$Morphir$Type$MetaType$MetaRef(
	A3($author$project$Morphir$IR$FQName$fqn, 'Morphir.SDK', 'String', 'String'));
var $author$project$Morphir$Type$Infer$constrainLiteral = F2(
	function (thisTypeVar, literalValue) {
		var expectExactType = function (expectedType) {
			return $author$project$Morphir$Type$ConstraintSet$singleton(
				A2(
					$author$project$Morphir$Type$Constraint$equality,
					$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
					expectedType));
		};
		switch (literalValue.$) {
			case 0:
				return expectExactType($author$project$Morphir$Type$MetaType$boolType);
			case 1:
				return expectExactType($author$project$Morphir$Type$MetaType$charType);
			case 2:
				return expectExactType($author$project$Morphir$Type$MetaType$stringType);
			case 3:
				return $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$class,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						0));
			default:
				return expectExactType($author$project$Morphir$Type$MetaType$floatType);
		}
	});
var $author$project$Morphir$Type$ConstraintSet$fromList = function (list) {
	return A3($elm$core$List$foldl, $author$project$Morphir$Type$ConstraintSet$insert, $author$project$Morphir$Type$ConstraintSet$empty, list);
};
var $author$project$Morphir$Type$MetaType$listType = function (itemType) {
	return A2(
		$author$project$Morphir$Type$MetaType$MetaApply,
		$author$project$Morphir$Type$MetaType$MetaRef(
			A3($author$project$Morphir$IR$FQName$fqn, 'Morphir.SDK', 'List', 'List')),
		itemType);
};
var $author$project$Morphir$Type$MetaTypeMapping$CouldNotFindConstructor = function (a) {
	return {$: 0, a: a};
};
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Morphir$Type$MetaTypeMapping$ctorToMetaType = F5(
	function (baseVar, refs, baseType, paramNames, ctorArgs) {
		var argVariables = A3(
			$elm$core$List$foldl,
			$elm$core$Set$union,
			$elm$core$Set$empty,
			A2($elm$core$List$map, $author$project$Morphir$IR$Type$collectVariables, ctorArgs));
		var allVariables = A2(
			$elm$core$Set$union,
			argVariables,
			$elm$core$Set$fromList(paramNames));
		var varToMeta = A2($author$project$Morphir$Type$MetaTypeMapping$concreteVarsToMetaVars, baseVar, allVariables);
		var recurse = function (cargs) {
			if (!cargs.b) {
				return A3(
					$elm$core$List$foldl,
					F2(
						function (paramName, metaTypeSoFar) {
							return A2(
								$author$project$Morphir$Type$MetaType$MetaApply,
								metaTypeSoFar,
								$author$project$Morphir$Type$MetaType$MetaVar(
									A2(
										$elm$core$Maybe$withDefault,
										baseVar,
										A2($elm$core$Dict$get, paramName, varToMeta))));
						}),
					baseType,
					paramNames);
			} else {
				var firstCtorArg = cargs.a;
				var restOfCtorArgs = cargs.b;
				return A2(
					$author$project$Morphir$Type$MetaType$MetaFun,
					A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta, firstCtorArg),
					recurse(restOfCtorArgs));
			}
		};
		return recurse(ctorArgs);
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Morphir$Type$MetaTypeMapping$lookupConstructor = F3(
	function (baseVar, refs, fQName) {
		var packageName = fQName.a;
		var moduleName = fQName.b;
		var localName = fQName.c;
		return A2(
			$elm$core$Result$fromMaybe,
			$author$project$Morphir$Type$MetaTypeMapping$CouldNotFindConstructor(fQName),
			A2(
				$elm$core$Maybe$andThen,
				function (moduleSpec) {
					return $elm$core$List$head(
						A2(
							$elm$core$List$concatMap,
							function (_v0) {
								var typeName = _v0.a;
								var typeSpec = _v0.b;
								var _v1 = typeSpec.aT;
								if (_v1.$ === 2) {
									var paramNames = _v1.a;
									var ctors = _v1.b;
									return A2(
										$elm$core$List$filterMap,
										function (_v2) {
											var ctorName = _v2.a;
											var ctorArgs = _v2.b;
											return _Utils_eq(ctorName, localName) ? $elm$core$Maybe$Just(
												A5(
													$author$project$Morphir$Type$MetaTypeMapping$ctorToMetaType,
													baseVar,
													refs,
													$author$project$Morphir$Type$MetaType$MetaRef(
														A3($author$project$Morphir$IR$FQName$FQName, packageName, moduleName, typeName)),
													paramNames,
													A2($elm$core$List$map, $elm$core$Tuple$second, ctorArgs))) : $elm$core$Maybe$Nothing;
										},
										ctors);
								} else {
									return _List_Nil;
								}
							},
							$elm$core$Dict$toList(moduleSpec.gn)));
				},
				A2(
					$elm$core$Maybe$andThen,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.e7;
						},
						$elm$core$Dict$get(moduleName)),
					A2($elm$core$Dict$get, packageName, refs))));
	});
var $author$project$Morphir$IR$Value$patternAttribute = function (p) {
	switch (p.$) {
		case 0:
			var a = p.a;
			return a;
		case 1:
			var a = p.a;
			return a;
		case 2:
			var a = p.a;
			return a;
		case 3:
			var a = p.a;
			return a;
		case 4:
			var a = p.a;
			return a;
		case 5:
			var a = p.a;
			return a;
		case 6:
			var a = p.a;
			return a;
		default:
			var a = p.a;
			return a;
	}
};
var $author$project$Morphir$Type$Infer$metaTypeVarForPattern = function (pattern) {
	return $author$project$Morphir$Type$MetaType$MetaVar(
		$author$project$Morphir$IR$Value$patternAttribute(pattern).b);
};
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $author$project$Morphir$Type$Infer$constrainPattern = F2(
	function (state, untypedPattern) {
		switch (untypedPattern.$) {
			case 0:
				return _Utils_Tuple2($elm$core$Dict$empty, $author$project$Morphir$Type$ConstraintSet$empty);
			case 1:
				var _v1 = untypedPattern.a;
				var thisTypeVar = _v1.b;
				var nestedPattern = untypedPattern.b;
				var alias = untypedPattern.c;
				var thisPatternConstraints = $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						$author$project$Morphir$Type$Infer$metaTypeVarForPattern(nestedPattern)));
				var _v2 = A2($author$project$Morphir$Type$Infer$constrainPattern, state, nestedPattern);
				var nestedVariables = _v2.a;
				var nestedConstraints = _v2.b;
				return _Utils_Tuple2(
					A3($elm$core$Dict$insert, alias, thisTypeVar, nestedVariables),
					A2($author$project$Morphir$Type$ConstraintSet$union, nestedConstraints, thisPatternConstraints));
			case 2:
				var _v3 = untypedPattern.a;
				var thisTypeVar = _v3.b;
				var elemPatterns = untypedPattern.b;
				var tupleConstraint = $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						$author$project$Morphir$Type$MetaType$MetaTuple(
							A2($elm$core$List$map, $author$project$Morphir$Type$Infer$metaTypeVarForPattern, elemPatterns))));
				var _v4 = $elm$core$List$unzip(
					A2(
						$elm$core$List$map,
						$author$project$Morphir$Type$Infer$constrainPattern(state),
						elemPatterns));
				var elemsVariables = _v4.a;
				var elemsConstraints = _v4.b;
				return _Utils_Tuple2(
					A3($elm$core$List$foldl, $elm$core$Dict$union, $elm$core$Dict$empty, elemsVariables),
					$author$project$Morphir$Type$ConstraintSet$concat(
						A2($elm$core$List$cons, tupleConstraint, elemsConstraints)));
			case 3:
				var _v5 = untypedPattern.a;
				var thisTypeVar = _v5.b;
				var fQName = untypedPattern.b;
				var argPatterns = untypedPattern.c;
				var resultType = function (t) {
					resultType:
					while (true) {
						if (t.$ === 5) {
							var a = t.a;
							var r = t.b;
							var $temp$t = r;
							t = $temp$t;
							continue resultType;
						} else {
							return t;
						}
					}
				};
				var ctorTypeVar = $author$project$Morphir$Type$MetaType$subVariable(thisTypeVar);
				var customTypeConstraint = A2(
					$elm$core$Result$withDefault,
					$author$project$Morphir$Type$ConstraintSet$empty,
					A2(
						$elm$core$Result$map,
						function (ctorFunType) {
							return $author$project$Morphir$Type$ConstraintSet$fromList(
								_List_fromArray(
									[
										A2(
										$author$project$Morphir$Type$Constraint$equality,
										$author$project$Morphir$Type$MetaType$MetaVar(ctorTypeVar),
										ctorFunType),
										A2(
										$author$project$Morphir$Type$Constraint$equality,
										$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
										resultType(ctorFunType))
									]));
						},
						A3($author$project$Morphir$Type$MetaTypeMapping$lookupConstructor, ctorTypeVar, state.ai, fQName)));
				var ctorType = function (args) {
					if (!args.b) {
						return $author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar);
					} else {
						var firstArg = args.a;
						var restOfArgs = args.b;
						return A2(
							$author$project$Morphir$Type$MetaType$MetaFun,
							firstArg,
							ctorType(restOfArgs));
					}
				};
				var ctorFunConstraint = $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(ctorTypeVar),
						ctorType(
							A2($elm$core$List$map, $author$project$Morphir$Type$Infer$metaTypeVarForPattern, argPatterns))));
				var _v8 = $elm$core$List$unzip(
					A2(
						$elm$core$List$map,
						$author$project$Morphir$Type$Infer$constrainPattern(state),
						argPatterns));
				var argVariables = _v8.a;
				var argConstraints = _v8.b;
				return _Utils_Tuple2(
					A3($elm$core$List$foldl, $elm$core$Dict$union, $elm$core$Dict$empty, argVariables),
					$author$project$Morphir$Type$ConstraintSet$concat(
						A2(
							$elm$core$List$cons,
							customTypeConstraint,
							A2($elm$core$List$cons, ctorFunConstraint, argConstraints))));
			case 4:
				var _v9 = untypedPattern.a;
				var thisTypeVar = _v9.b;
				var itemType = $author$project$Morphir$Type$MetaType$MetaVar(
					$author$project$Morphir$Type$MetaType$subVariable(thisTypeVar));
				var listType = $author$project$Morphir$Type$MetaType$listType(itemType);
				return _Utils_Tuple2(
					$elm$core$Dict$empty,
					$author$project$Morphir$Type$ConstraintSet$singleton(
						A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							listType)));
			case 5:
				var _v10 = untypedPattern.a;
				var thisTypeVar = _v10.b;
				var headPattern = untypedPattern.b;
				var tailPattern = untypedPattern.c;
				var itemType = $author$project$Morphir$Type$Infer$metaTypeVarForPattern(headPattern);
				var listType = $author$project$Morphir$Type$MetaType$listType(itemType);
				var thisPatternConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							listType),
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$Infer$metaTypeVarForPattern(tailPattern),
							listType)
						]));
				var _v11 = A2($author$project$Morphir$Type$Infer$constrainPattern, state, tailPattern);
				var tailVariables = _v11.a;
				var tailConstraints = _v11.b;
				var _v12 = A2($author$project$Morphir$Type$Infer$constrainPattern, state, headPattern);
				var headVariables = _v12.a;
				var headConstraints = _v12.b;
				return _Utils_Tuple2(
					A2($elm$core$Dict$union, headVariables, tailVariables),
					$author$project$Morphir$Type$ConstraintSet$concat(
						_List_fromArray(
							[headConstraints, tailConstraints, thisPatternConstraints])));
			case 6:
				var _v13 = untypedPattern.a;
				var thisTypeVar = _v13.b;
				var literalValue = untypedPattern.b;
				return _Utils_Tuple2(
					$elm$core$Dict$empty,
					A2($author$project$Morphir$Type$Infer$constrainLiteral, thisTypeVar, literalValue));
			default:
				var _v14 = untypedPattern.a;
				var thisTypeVar = _v14.b;
				return _Utils_Tuple2(
					$elm$core$Dict$empty,
					$author$project$Morphir$Type$ConstraintSet$singleton(
						A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							$author$project$Morphir$Type$MetaType$MetaUnit)));
		}
	});
var $author$project$Morphir$Type$MetaTypeMapping$CouldNotFindValue = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Type$MetaTypeMapping$valueSpecToMetaType = F3(
	function (baseVar, refs, valueSpec) {
		var specToFunctionType = F2(
			function (argTypes, returnType) {
				if (!argTypes.b) {
					return returnType;
				} else {
					var firstArg = argTypes.a;
					var restOfArgs = argTypes.b;
					return A3(
						$author$project$Morphir$IR$Type$Function,
						0,
						firstArg,
						A2(specToFunctionType, restOfArgs, returnType));
				}
			});
		var functionType = A2(
			specToFunctionType,
			A2($elm$core$List$map, $elm$core$Tuple$second, valueSpec.bo),
			valueSpec.bN);
		var varToMeta = A2(
			$author$project$Morphir$Type$MetaTypeMapping$concreteVarsToMetaVars,
			baseVar,
			$author$project$Morphir$IR$Type$collectVariables(functionType));
		return A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, refs, varToMeta, functionType);
	});
var $author$project$Morphir$Type$MetaTypeMapping$lookupValue = F3(
	function (baseVar, refs, fQName) {
		var packageName = fQName.a;
		var moduleName = fQName.b;
		var localName = fQName.c;
		return A2(
			$elm$core$Result$fromMaybe,
			$author$project$Morphir$Type$MetaTypeMapping$CouldNotFindValue(fQName),
			A2(
				$elm$core$Maybe$map,
				A2($author$project$Morphir$Type$MetaTypeMapping$valueSpecToMetaType, baseVar, refs),
				A2(
					$elm$core$Maybe$andThen,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.gq;
						},
						$elm$core$Dict$get(localName)),
					A2(
						$elm$core$Maybe$andThen,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.e7;
							},
							$elm$core$Dict$get(moduleName)),
						A2($elm$core$Dict$get, packageName, refs)))));
	});
var $author$project$Morphir$IR$Value$valueAttribute = function (v) {
	switch (v.$) {
		case 0:
			var a = v.a;
			return a;
		case 1:
			var a = v.a;
			return a;
		case 2:
			var a = v.a;
			return a;
		case 3:
			var a = v.a;
			return a;
		case 4:
			var a = v.a;
			return a;
		case 5:
			var a = v.a;
			return a;
		case 6:
			var a = v.a;
			return a;
		case 7:
			var a = v.a;
			return a;
		case 8:
			var a = v.a;
			return a;
		case 9:
			var a = v.a;
			return a;
		case 10:
			var a = v.a;
			return a;
		case 11:
			var a = v.a;
			return a;
		case 12:
			var a = v.a;
			return a;
		case 13:
			var a = v.a;
			return a;
		case 14:
			var a = v.a;
			return a;
		case 15:
			var a = v.a;
			return a;
		case 16:
			var a = v.a;
			return a;
		default:
			var a = v.a;
			return a;
	}
};
var $author$project$Morphir$Type$Infer$metaTypeVarForValue = function (value) {
	return $author$project$Morphir$Type$MetaType$MetaVar(
		$author$project$Morphir$IR$Value$valueAttribute(value).b);
};
var $author$project$Morphir$Type$Infer$constrainDefinition = F3(
	function (baseVar, state, def) {
		var outputTypeVars = $author$project$Morphir$IR$Type$collectVariables(def.fq);
		var inputVars = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (_v41) {
					var name = _v41.a;
					var _v42 = _v41.b;
					var thisTypeVar = _v42.b;
					return _Utils_Tuple2(name, thisTypeVar);
				},
				def.a3));
		var inputTypeVars = A3(
			$elm$core$List$foldl,
			$elm$core$Set$union,
			$elm$core$Set$empty,
			A2(
				$elm$core$List$map,
				function (_v40) {
					var declaredType = _v40.c;
					return $author$project$Morphir$IR$Type$collectVariables(declaredType);
				},
				def.a3));
		var varToMeta = A2(
			$author$project$Morphir$Type$MetaTypeMapping$concreteVarsToMetaVars,
			baseVar,
			A2($elm$core$Set$union, inputTypeVars, outputTypeVars));
		var outputConstraints = $author$project$Morphir$Type$ConstraintSet$singleton(
			A2(
				$author$project$Morphir$Type$Constraint$equality,
				$author$project$Morphir$Type$Infer$metaTypeVarForValue(def.dV),
				A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, baseVar, state.ai, varToMeta, def.fq)));
		var inputConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
			A2(
				$elm$core$List$map,
				function (_v38) {
					var _v39 = _v38.b;
					var thisTypeVar = _v39.b;
					var declaredType = _v38.c;
					return A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						A4($author$project$Morphir$Type$MetaTypeMapping$concreteTypeToMetaType, thisTypeVar, state.ai, varToMeta, declaredType));
				},
				def.a3));
		var bodyConstraints = A2(
			$author$project$Morphir$Type$Infer$constrainValue,
			_Utils_update(
				state,
				{
					z: A2($elm$core$Dict$union, inputVars, state.z)
				}),
			def.dV);
		return $author$project$Morphir$Type$ConstraintSet$concat(
			_List_fromArray(
				[inputConstraints, outputConstraints, bodyConstraints]));
	});
var $author$project$Morphir$Type$Infer$constrainValue = F2(
	function (state, annotatedValue) {
		switch (annotatedValue.$) {
			case 0:
				var _v1 = annotatedValue.a;
				var thisTypeVar = _v1.b;
				var literalValue = annotatedValue.b;
				return A2($author$project$Morphir$Type$Infer$constrainLiteral, thisTypeVar, literalValue);
			case 1:
				var _v2 = annotatedValue.a;
				var thisTypeVar = _v2.b;
				var fQName = annotatedValue.b;
				return A2(
					$elm$core$Result$withDefault,
					$author$project$Morphir$Type$ConstraintSet$empty,
					A2(
						$elm$core$Result$map,
						$author$project$Morphir$Type$ConstraintSet$singleton,
						A2(
							$elm$core$Result$map,
							$author$project$Morphir$Type$Constraint$equality(
								$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar)),
							A3($author$project$Morphir$Type$MetaTypeMapping$lookupConstructor, thisTypeVar, state.ai, fQName))));
			case 2:
				var _v3 = annotatedValue.a;
				var thisTypeVar = _v3.b;
				var elems = annotatedValue.b;
				var tupleConstraint = $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						$author$project$Morphir$Type$MetaType$MetaTuple(
							A2($elm$core$List$map, $author$project$Morphir$Type$Infer$metaTypeVarForValue, elems))));
				var elemsConstraints = A2(
					$elm$core$List$map,
					$author$project$Morphir$Type$Infer$constrainValue(state),
					elems);
				return $author$project$Morphir$Type$ConstraintSet$concat(
					A2($elm$core$List$cons, tupleConstraint, elemsConstraints));
			case 3:
				var _v4 = annotatedValue.a;
				var thisTypeVar = _v4.b;
				var items = annotatedValue.b;
				var itemType = $author$project$Morphir$Type$MetaType$MetaVar(
					$author$project$Morphir$Type$MetaType$subVariable(thisTypeVar));
				var listConstraint = A2(
					$author$project$Morphir$Type$Constraint$equality,
					$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
					$author$project$Morphir$Type$MetaType$listType(itemType));
				var itemConstraints = $author$project$Morphir$Type$ConstraintSet$concat(
					A2(
						$elm$core$List$map,
						function (item) {
							return A2(
								$author$project$Morphir$Type$ConstraintSet$insert,
								A2(
									$author$project$Morphir$Type$Constraint$equality,
									$author$project$Morphir$Type$Infer$metaTypeVarForValue(item),
									itemType),
								A2($author$project$Morphir$Type$Infer$constrainValue, state, item));
						},
						items));
				return A2($author$project$Morphir$Type$ConstraintSet$insert, listConstraint, itemConstraints);
			case 4:
				var _v5 = annotatedValue.a;
				var thisTypeVar = _v5.b;
				var fieldValues = annotatedValue.b;
				var recordType = A2(
					$author$project$Morphir$Type$MetaType$MetaRecord,
					$elm$core$Maybe$Nothing,
					$elm$core$Dict$fromList(
						A2(
							$elm$core$List$map,
							function (_v6) {
								var fieldName = _v6.a;
								var fieldValue = _v6.b;
								return _Utils_Tuple2(
									fieldName,
									$author$project$Morphir$Type$Infer$metaTypeVarForValue(fieldValue));
							},
							fieldValues)));
				var recordConstraints = $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						recordType));
				var fieldConstraints = $author$project$Morphir$Type$ConstraintSet$concat(
					A2(
						$elm$core$List$map,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$second,
							$author$project$Morphir$Type$Infer$constrainValue(state)),
						fieldValues));
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[fieldConstraints, recordConstraints]));
			case 5:
				var _v7 = annotatedValue.a;
				var varUse = _v7.b;
				var varName = annotatedValue.b;
				var _v8 = A2($elm$core$Dict$get, varName, state.z);
				if (!_v8.$) {
					var varDecl = _v8.a;
					return $author$project$Morphir$Type$ConstraintSet$singleton(
						A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(varUse),
							$author$project$Morphir$Type$MetaType$MetaVar(varDecl)));
				} else {
					return $author$project$Morphir$Type$ConstraintSet$empty;
				}
			case 6:
				var _v9 = annotatedValue.a;
				var thisTypeVar = _v9.b;
				var fQName = annotatedValue.b;
				return A2(
					$elm$core$Result$withDefault,
					$author$project$Morphir$Type$ConstraintSet$empty,
					A2(
						$elm$core$Result$map,
						$author$project$Morphir$Type$ConstraintSet$singleton,
						A2(
							$elm$core$Result$map,
							$author$project$Morphir$Type$Constraint$equality(
								$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar)),
							A3($author$project$Morphir$Type$MetaTypeMapping$lookupValue, thisTypeVar, state.ai, fQName))));
			case 7:
				var _v10 = annotatedValue.a;
				var thisTypeVar = _v10.b;
				var subjectValue = annotatedValue.b;
				var fieldName = annotatedValue.c;
				var extendsVar = $author$project$Morphir$Type$MetaType$subVariable(thisTypeVar);
				var fieldType = $author$project$Morphir$Type$MetaType$MetaVar(
					$author$project$Morphir$Type$MetaType$subVariable(extendsVar));
				var extensibleRecordType = A2(
					$author$project$Morphir$Type$MetaType$MetaRecord,
					$elm$core$Maybe$Just(extendsVar),
					A2($elm$core$Dict$singleton, fieldName, fieldType));
				var fieldConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(subjectValue),
							extensibleRecordType),
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							fieldType)
						]));
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[
							A2($author$project$Morphir$Type$Infer$constrainValue, state, subjectValue),
							fieldConstraints
						]));
			case 8:
				var _v11 = annotatedValue.a;
				var thisTypeVar = _v11.b;
				var fieldName = annotatedValue.b;
				var extendsVar = $author$project$Morphir$Type$MetaType$subVariable(thisTypeVar);
				var fieldType = $author$project$Morphir$Type$MetaType$MetaVar(
					$author$project$Morphir$Type$MetaType$subVariable(extendsVar));
				var extensibleRecordType = A2(
					$author$project$Morphir$Type$MetaType$MetaRecord,
					$elm$core$Maybe$Just(extendsVar),
					A2($elm$core$Dict$singleton, fieldName, fieldType));
				return $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						A2($author$project$Morphir$Type$MetaType$MetaFun, extensibleRecordType, fieldType)));
			case 9:
				var _v12 = annotatedValue.a;
				var thisTypeVar = _v12.b;
				var funValue = annotatedValue.b;
				var argValue = annotatedValue.c;
				var funType = A2(
					$author$project$Morphir$Type$MetaType$MetaFun,
					$author$project$Morphir$Type$Infer$metaTypeVarForValue(argValue),
					$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar));
				var applyConstraints = $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$Infer$metaTypeVarForValue(funValue),
						funType));
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[
							A2($author$project$Morphir$Type$Infer$constrainValue, state, funValue),
							A2($author$project$Morphir$Type$Infer$constrainValue, state, argValue),
							applyConstraints
						]));
			case 10:
				var _v13 = annotatedValue.a;
				var thisTypeVar = _v13.b;
				var argPattern = annotatedValue.b;
				var bodyValue = annotatedValue.c;
				var lambdaType = A2(
					$author$project$Morphir$Type$MetaType$MetaFun,
					$author$project$Morphir$Type$Infer$metaTypeVarForPattern(argPattern),
					$author$project$Morphir$Type$Infer$metaTypeVarForValue(bodyValue));
				var lambdaConstraints = $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						lambdaType));
				var _v14 = A2($author$project$Morphir$Type$Infer$constrainPattern, state, argPattern);
				var argVariables = _v14.a;
				var argConstraints = _v14.b;
				var bodyConstraints = A2(
					$author$project$Morphir$Type$Infer$constrainValue,
					_Utils_update(
						state,
						{
							z: A2($elm$core$Dict$union, argVariables, state.z)
						}),
					bodyValue);
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[lambdaConstraints, bodyConstraints, argConstraints]));
			case 11:
				var _v15 = annotatedValue.a;
				var thisTypeVar = _v15.b;
				var defName = annotatedValue.b;
				var def = annotatedValue.c;
				var inValue = annotatedValue.d;
				var defTypeVar = $author$project$Morphir$Type$MetaType$subVariable(thisTypeVar);
				var inConstraints = A2(
					$author$project$Morphir$Type$Infer$constrainValue,
					_Utils_update(
						state,
						{
							z: A3($elm$core$Dict$insert, defName, defTypeVar, state.z)
						}),
					inValue);
				var defType = F2(
					function (argTypes, returnType) {
						if (!argTypes.b) {
							return returnType;
						} else {
							var firstArg = argTypes.a;
							var restOfArgs = argTypes.b;
							return A2(
								$author$project$Morphir$Type$MetaType$MetaFun,
								firstArg,
								A2(defType, restOfArgs, returnType));
						}
					});
				var letConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(inValue)),
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(defTypeVar),
							A2(
								defType,
								A2(
									$elm$core$List$map,
									function (_v17) {
										var _v18 = _v17.b;
										var argTypeVar = _v18.b;
										return $author$project$Morphir$Type$MetaType$MetaVar(argTypeVar);
									},
									def.a3),
								$author$project$Morphir$Type$Infer$metaTypeVarForValue(def.dV)))
						]));
				var defConstraints = A3($author$project$Morphir$Type$Infer$constrainDefinition, thisTypeVar, state, def);
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[defConstraints, inConstraints, letConstraints]));
			case 12:
				var _v19 = annotatedValue.a;
				var thisTypeVar = _v19.b;
				var defs = annotatedValue.b;
				var inValue = annotatedValue.c;
				var letConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(inValue))
						]));
				var defType = F2(
					function (argTypes, returnType) {
						if (!argTypes.b) {
							return returnType;
						} else {
							var firstArg = argTypes.a;
							var restOfArgs = argTypes.b;
							return A2(
								$author$project$Morphir$Type$MetaType$MetaFun,
								firstArg,
								A2(defType, restOfArgs, returnType));
						}
					});
				var _v21 = A3(
					$elm$core$List$foldl,
					F2(
						function (_v22, _v23) {
							var defName = _v22.a;
							var def = _v22.b;
							var lastTypeVar = _v23.a;
							var constraintsSoFar = _v23.b;
							var variablesSoFar = _v23.c;
							var nextTypeVar = $author$project$Morphir$Type$MetaType$subVariable(lastTypeVar);
							var letConstraint = $author$project$Morphir$Type$ConstraintSet$fromList(
								_List_fromArray(
									[
										A2(
										$author$project$Morphir$Type$Constraint$equality,
										$author$project$Morphir$Type$MetaType$MetaVar(nextTypeVar),
										A2(
											defType,
											A2(
												$elm$core$List$map,
												function (_v24) {
													var _v25 = _v24.b;
													var argTypeVar = _v25.b;
													return $author$project$Morphir$Type$MetaType$MetaVar(argTypeVar);
												},
												def.a3),
											$author$project$Morphir$Type$Infer$metaTypeVarForValue(def.dV)))
									]));
							return _Utils_Tuple3(
								nextTypeVar,
								A2($elm$core$List$cons, letConstraint, constraintsSoFar),
								A2(
									$elm$core$List$cons,
									_Utils_Tuple2(defName, nextTypeVar),
									variablesSoFar));
						}),
					_Utils_Tuple3(thisTypeVar, _List_Nil, _List_Nil),
					$elm$core$Dict$toList(defs));
				var lastDefTypeVar = _v21.a;
				var defDeclsConstraints = _v21.b;
				var defVariables = _v21.c;
				var inConstraints = A2(
					$author$project$Morphir$Type$Infer$constrainValue,
					_Utils_update(
						state,
						{
							z: A2(
								$elm$core$Dict$union,
								$elm$core$Dict$fromList(defVariables),
								state.z)
						}),
					inValue);
				var defsConstraints = $author$project$Morphir$Type$ConstraintSet$concat(
					A3(
						$elm$core$List$foldl,
						F2(
							function (_v26, _v27) {
								var def = _v26.b;
								var lastTypeVar = _v27.a;
								var constraintsSoFar = _v27.b;
								var nextTypeVar = $author$project$Morphir$Type$MetaType$subVariable(lastTypeVar);
								var defConstraints = A3($author$project$Morphir$Type$Infer$constrainDefinition, lastTypeVar, state, def);
								return _Utils_Tuple2(
									nextTypeVar,
									A2($elm$core$List$cons, defConstraints, constraintsSoFar));
							}),
						_Utils_Tuple2(lastDefTypeVar, defDeclsConstraints),
						$elm$core$Dict$toList(defs)).b);
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[defsConstraints, inConstraints, letConstraints]));
			case 13:
				var _v28 = annotatedValue.a;
				var thisTypeVar = _v28.b;
				var bindPattern = annotatedValue.b;
				var bindValue = annotatedValue.c;
				var inValue = annotatedValue.d;
				var destructureConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(inValue)),
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(bindValue),
							$author$project$Morphir$Type$Infer$metaTypeVarForPattern(bindPattern))
						]));
				var bindValueConstraints = A2($author$project$Morphir$Type$Infer$constrainValue, state, bindValue);
				var _v29 = A2($author$project$Morphir$Type$Infer$constrainPattern, state, bindPattern);
				var bindPatternVariables = _v29.a;
				var bindPatternConstraints = _v29.b;
				var inValueConstraints = A2(
					$author$project$Morphir$Type$Infer$constrainValue,
					_Utils_update(
						state,
						{
							z: A2($elm$core$Dict$union, bindPatternVariables, state.z)
						}),
					inValue);
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[bindPatternConstraints, bindValueConstraints, inValueConstraints, destructureConstraints]));
			case 14:
				var _v30 = annotatedValue.a;
				var thisTypeVar = _v30.b;
				var condition = annotatedValue.b;
				var thenBranch = annotatedValue.c;
				var elseBranch = annotatedValue.d;
				var specificConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(condition),
							$author$project$Morphir$Type$MetaType$boolType),
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(elseBranch),
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(thenBranch)),
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(thenBranch))
						]));
				var childConstraints = _List_fromArray(
					[
						A2($author$project$Morphir$Type$Infer$constrainValue, state, condition),
						A2($author$project$Morphir$Type$Infer$constrainValue, state, thenBranch),
						A2($author$project$Morphir$Type$Infer$constrainValue, state, elseBranch)
					]);
				return $author$project$Morphir$Type$ConstraintSet$concat(
					A2($elm$core$List$cons, specificConstraints, childConstraints));
			case 15:
				var _v31 = annotatedValue.a;
				var thisTypeVar = _v31.b;
				var subjectValue = annotatedValue.b;
				var cases = annotatedValue.c;
				var thisType = $author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar);
				var subjectType = $author$project$Morphir$Type$Infer$metaTypeVarForValue(subjectValue);
				var subjectConstraints = A2($author$project$Morphir$Type$Infer$constrainValue, state, subjectValue);
				var casesConstraints = A2(
					$elm$core$List$map,
					function (_v32) {
						var casePattern = _v32.a;
						var caseValue = _v32.b;
						var caseConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
							_List_fromArray(
								[
									A2(
									$author$project$Morphir$Type$Constraint$equality,
									subjectType,
									$author$project$Morphir$Type$Infer$metaTypeVarForPattern(casePattern)),
									A2(
									$author$project$Morphir$Type$Constraint$equality,
									thisType,
									$author$project$Morphir$Type$Infer$metaTypeVarForValue(caseValue))
								]));
						var _v33 = A2($author$project$Morphir$Type$Infer$constrainPattern, state, casePattern);
						var casePatternVariables = _v33.a;
						var casePatternConstraints = _v33.b;
						var caseValueConstraints = A2(
							$author$project$Morphir$Type$Infer$constrainValue,
							_Utils_update(
								state,
								{
									z: A2($elm$core$Dict$union, casePatternVariables, state.z)
								}),
							caseValue);
						return $author$project$Morphir$Type$ConstraintSet$concat(
							_List_fromArray(
								[casePatternConstraints, caseValueConstraints, caseConstraints]));
					},
					cases);
				return $author$project$Morphir$Type$ConstraintSet$concat(
					A2($elm$core$List$cons, subjectConstraints, casesConstraints));
			case 16:
				var _v34 = annotatedValue.a;
				var thisTypeVar = _v34.b;
				var subjectValue = annotatedValue.b;
				var fieldValues = annotatedValue.c;
				var fieldValueConstraints = $author$project$Morphir$Type$ConstraintSet$concat(
					A2(
						$elm$core$List$map,
						function (_v36) {
							var fieldValue = _v36.b;
							return A2($author$project$Morphir$Type$Infer$constrainValue, state, fieldValue);
						},
						fieldValues));
				var extendsVar = $author$project$Morphir$Type$MetaType$subVariable(thisTypeVar);
				var extensibleRecordType = A2(
					$author$project$Morphir$Type$MetaType$MetaRecord,
					$elm$core$Maybe$Just(extendsVar),
					$elm$core$Dict$fromList(
						A2(
							$elm$core$List$map,
							function (_v35) {
								var fieldName = _v35.a;
								var fieldValue = _v35.b;
								return _Utils_Tuple2(
									fieldName,
									$author$project$Morphir$Type$Infer$metaTypeVarForValue(fieldValue));
							},
							fieldValues)));
				var fieldConstraints = $author$project$Morphir$Type$ConstraintSet$fromList(
					_List_fromArray(
						[
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(subjectValue),
							extensibleRecordType),
							A2(
							$author$project$Morphir$Type$Constraint$equality,
							$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
							$author$project$Morphir$Type$Infer$metaTypeVarForValue(subjectValue))
						]));
				return $author$project$Morphir$Type$ConstraintSet$concat(
					_List_fromArray(
						[
							A2($author$project$Morphir$Type$Infer$constrainValue, state, subjectValue),
							fieldValueConstraints,
							fieldConstraints
						]));
			default:
				var _v37 = annotatedValue.a;
				var thisTypeVar = _v37.b;
				return $author$project$Morphir$Type$ConstraintSet$singleton(
					A2(
						$author$project$Morphir$Type$Constraint$equality,
						$author$project$Morphir$Type$MetaType$MetaVar(thisTypeVar),
						$author$project$Morphir$Type$MetaType$MetaUnit));
		}
	});
var $author$project$Morphir$Type$SolutionMap$SolutionMap = $elm$core$Basics$identity;
var $author$project$Morphir$Type$SolutionMap$empty = $elm$core$Dict$empty;
var $author$project$Morphir$Type$MetaType$substituteVariable = F3(
	function (_var, replacement, original) {
		switch (original.$) {
			case 0:
				var thisVar = original.a;
				return _Utils_eq(thisVar, _var) ? replacement : original;
			case 2:
				var metaElems = original.a;
				return $author$project$Morphir$Type$MetaType$MetaTuple(
					A2(
						$elm$core$List$map,
						A2($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement),
						metaElems));
			case 3:
				var _extends = original.a;
				var metaFields = original.b;
				return _Utils_eq(
					_extends,
					$elm$core$Maybe$Just(_var)) ? replacement : A2(
					$author$project$Morphir$Type$MetaType$MetaRecord,
					_extends,
					A2(
						$elm$core$Dict$map,
						F2(
							function (_v1, fieldType) {
								return A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, fieldType);
							}),
						metaFields));
			case 4:
				var metaFun = original.a;
				var metaArg = original.b;
				return A2(
					$author$project$Morphir$Type$MetaType$MetaApply,
					A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaFun),
					A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaArg));
			case 5:
				var metaFun = original.a;
				var metaArg = original.b;
				return A2(
					$author$project$Morphir$Type$MetaType$MetaFun,
					A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaFun),
					A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaArg));
			case 1:
				return original;
			case 6:
				return original;
			default:
				var alias = original.a;
				var subject = original.b;
				return A2(
					$author$project$Morphir$Type$MetaType$MetaAlias,
					alias,
					A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, subject));
		}
	});
var $author$project$Morphir$Type$Constraint$substitute = F3(
	function (_var, replacement, constraint) {
		if (!constraint.$) {
			var metaType1 = constraint.a;
			var metaType2 = constraint.b;
			return A2(
				$author$project$Morphir$Type$Constraint$Equality,
				A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaType1),
				A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaType2));
		} else {
			var metaType = constraint.a;
			var cls = constraint.b;
			return A2(
				$author$project$Morphir$Type$Constraint$Class,
				A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaType),
				cls);
		}
	});
var $author$project$Morphir$Type$ConstraintSet$substituteVariable = F3(
	function (_var, replacement, _v0) {
		var constraints = _v0;
		return A2(
			$elm$core$List$map,
			A2($author$project$Morphir$Type$Constraint$substitute, _var, replacement),
			constraints);
	});
var $author$project$Morphir$Type$ConstraintSet$applySubstitutions = F2(
	function (_v0, constraintSet) {
		var substitutions = _v0;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v1, soFar) {
					var _var = _v1.a;
					var replacement = _v1.b;
					return A3($author$project$Morphir$Type$ConstraintSet$substituteVariable, _var, replacement, soFar);
				}),
			constraintSet,
			$elm$core$Dict$toList(substitutions));
	});
var $author$project$Morphir$Type$SolutionMap$isEmpty = function (_v0) {
	var solutions = _v0;
	return $elm$core$Dict$isEmpty(solutions);
};
var $author$project$Morphir$Type$Infer$CouldNotUnify = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $author$project$Morphir$Type$Infer$FieldMismatch = 3;
var $author$project$Morphir$Type$Infer$NoUnificationRule = 0;
var $author$project$Morphir$Type$Infer$RefMismatch = 2;
var $author$project$Morphir$Type$Infer$TuplesOfDifferentSize = 1;
var $author$project$Morphir$Type$Infer$TypeErrors = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Type$Infer$UnknownError = function (a) {
	return {$: 3, a: a};
};
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			$elm$core$Dict$filter,
			F2(
				function (k, _v0) {
					return A2($elm$core$Dict$member, k, t2);
				}),
			t1);
	});
var $elm$core$Result$toMaybe = function (result) {
	if (!result.$) {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Morphir$ListOfResults$liftAllErrors = function (results) {
	var oks = A2(
		$elm$core$List$filterMap,
		function (result) {
			return $elm$core$Result$toMaybe(result);
		},
		results);
	var errs = A2(
		$elm$core$List$filterMap,
		function (result) {
			if (!result.$) {
				return $elm$core$Maybe$Nothing;
			} else {
				var e = result.a;
				return $elm$core$Maybe$Just(e);
			}
		},
		results);
	if (!errs.b) {
		return $elm$core$Result$Ok(oks);
	} else {
		return $elm$core$Result$Err(errs);
	}
};
var $elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 1) {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 1) {
				var x = rb.a;
				return $elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return $elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Morphir$Type$SolutionMap$singleton = F2(
	function (_var, metaType) {
		return A2($elm$core$Dict$singleton, _var, metaType);
	});
var $author$project$Morphir$Type$SolutionMap$substituteVariable = F3(
	function (_var, replacement, _v0) {
		var solutions = _v0;
		return A2(
			$elm$core$Dict$map,
			F2(
				function (_v1, metaType) {
					return A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, metaType);
				}),
			solutions);
	});
var $author$project$Morphir$Type$MetaType$substituteVariables = F2(
	function (replacements, original) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, soFar) {
					var _var = _v0.a;
					var replacement = _v0.b;
					return A3($author$project$Morphir$Type$MetaType$substituteVariable, _var, replacement, soFar);
				}),
			original,
			replacements);
	});
var $elm$core$Char$toUpper = _Char_toUpper;
var $author$project$Morphir$IR$Name$capitalize = function (string) {
	var _v0 = $elm$core$String$uncons(string);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var headChar = _v1.a;
		var tailString = _v1.b;
		return A2(
			$elm$core$String$cons,
			$elm$core$Char$toUpper(headChar),
			tailString);
	} else {
		return string;
	}
};
var $author$project$Morphir$IR$Name$toCamelCase = function (name) {
	var _v0 = $author$project$Morphir$IR$Name$toList(name);
	if (!_v0.b) {
		return '';
	} else {
		var head = _v0.a;
		var tail = _v0.b;
		return A2(
			$elm$core$String$join,
			'',
			A2(
				$elm$core$List$cons,
				head,
				A2($elm$core$List$map, $author$project$Morphir$IR$Name$capitalize, tail)));
	}
};
var $author$project$Morphir$Type$Infer$typeErrors = function (errors) {
	if (errors.b && (!errors.b.b)) {
		var single = errors.a;
		return single;
	} else {
		return $author$project$Morphir$Type$Infer$TypeErrors(errors);
	}
};
var $author$project$Morphir$Type$Infer$wrapInAliases = F2(
	function (aliases, metaType) {
		if (!aliases.b) {
			return metaType;
		} else {
			var firstAlias = aliases.a;
			var restOfAliases = aliases.b;
			return A2(
				$author$project$Morphir$Type$MetaType$MetaAlias,
				firstAlias,
				A2($author$project$Morphir$Type$Infer$wrapInAliases, restOfAliases, metaType));
		}
	});
var $author$project$Morphir$Type$Infer$unifyVariable = F3(
	function (aliases, var1, metaType2) {
		return $elm$core$Result$Ok(
			A2(
				$author$project$Morphir$Type$SolutionMap$singleton,
				var1,
				A2($author$project$Morphir$Type$Infer$wrapInAliases, aliases, metaType2)));
	});
var $author$project$Morphir$Type$Infer$unifyUnit = F2(
	function (aliases, metaType2) {
		unifyUnit:
		while (true) {
			switch (metaType2.$) {
				case 7:
					var alias = metaType2.a;
					var subject2 = metaType2.b;
					var $temp$aliases = A2($elm$core$List$cons, alias, aliases),
						$temp$metaType2 = subject2;
					aliases = $temp$aliases;
					metaType2 = $temp$metaType2;
					continue unifyUnit;
				case 0:
					var var2 = metaType2.a;
					return A3($author$project$Morphir$Type$Infer$unifyVariable, aliases, var2, $author$project$Morphir$Type$MetaType$MetaUnit);
				case 6:
					return $elm$core$Result$Ok($author$project$Morphir$Type$SolutionMap$empty);
				default:
					return $elm$core$Result$Err(
						A3($author$project$Morphir$Type$Infer$CouldNotUnify, 0, $author$project$Morphir$Type$MetaType$MetaUnit, metaType2));
			}
		}
	});
var $author$project$Morphir$Type$Infer$addSolution = F5(
	function (baseVar, refs, _var, newSolution, _v12) {
		var currentSolutions = _v12;
		var _v13 = A2($elm$core$Dict$get, _var, currentSolutions);
		if (!_v13.$) {
			var existingSolution = _v13.a;
			return A2(
				$elm$core$Result$map,
				function (_v14) {
					var newSubstitutions = _v14;
					return A2(
						$elm$core$Dict$union,
						newSubstitutions,
						A3(
							$elm$core$Dict$insert,
							_var,
							A2(
								$author$project$Morphir$Type$MetaType$substituteVariables,
								$elm$core$Dict$toList(newSubstitutions),
								existingSolution),
							currentSolutions));
				},
				A5($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, _List_Nil, existingSolution, newSolution));
		} else {
			return $elm$core$Result$Ok(
				A3(
					$author$project$Morphir$Type$SolutionMap$substituteVariable,
					_var,
					newSolution,
					A3($elm$core$Dict$insert, _var, newSolution, currentSolutions)));
		}
	});
var $author$project$Morphir$Type$Infer$concatSolutions = F3(
	function (baseVar, refs, solutionMaps) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (nextSolutions, resultSoFar) {
					return A2(
						$elm$core$Result$andThen,
						function (solutionsSoFar) {
							return A4($author$project$Morphir$Type$Infer$mergeSolutions, baseVar, refs, solutionsSoFar, nextSolutions);
						},
						resultSoFar);
				}),
			$elm$core$Result$Ok($author$project$Morphir$Type$SolutionMap$empty),
			solutionMaps);
	});
var $author$project$Morphir$Type$Infer$mergeSolutions = F4(
	function (baseVar, refs, _v10, currentSolutions) {
		var newSolutions = _v10;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v11, solutionsSoFar) {
					var _var = _v11.a;
					var newSolution = _v11.b;
					return A2(
						$elm$core$Result$andThen,
						A4($author$project$Morphir$Type$Infer$addSolution, baseVar, refs, _var, newSolution),
						solutionsSoFar);
				}),
			$elm$core$Result$Ok(currentSolutions),
			$elm$core$Dict$toList(newSolutions));
	});
var $author$project$Morphir$Type$Infer$unifyApply = F6(
	function (baseVar, refs, aliases, fun1, arg1, metaType2) {
		unifyApply:
		while (true) {
			switch (metaType2.$) {
				case 7:
					var alias = metaType2.a;
					var subject2 = metaType2.b;
					var $temp$baseVar = baseVar,
						$temp$refs = refs,
						$temp$aliases = A2($elm$core$List$cons, alias, aliases),
						$temp$fun1 = fun1,
						$temp$arg1 = arg1,
						$temp$metaType2 = subject2;
					baseVar = $temp$baseVar;
					refs = $temp$refs;
					aliases = $temp$aliases;
					fun1 = $temp$fun1;
					arg1 = $temp$arg1;
					metaType2 = $temp$metaType2;
					continue unifyApply;
				case 0:
					var var2 = metaType2.a;
					return A3(
						$author$project$Morphir$Type$Infer$unifyVariable,
						aliases,
						var2,
						A2($author$project$Morphir$Type$MetaType$MetaApply, fun1, arg1));
				case 4:
					var fun2 = metaType2.a;
					var arg2 = metaType2.b;
					return A2(
						$elm$core$Result$andThen,
						$elm$core$Basics$identity,
						A3(
							$elm$core$Result$map2,
							A2($author$project$Morphir$Type$Infer$mergeSolutions, baseVar, refs),
							A5($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, aliases, fun1, fun2),
							A5($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, aliases, arg1, arg2)));
				default:
					return $elm$core$Result$Err(
						A3(
							$author$project$Morphir$Type$Infer$CouldNotUnify,
							0,
							A2($author$project$Morphir$Type$MetaType$MetaApply, fun1, arg1),
							metaType2));
			}
		}
	});
var $author$project$Morphir$Type$Infer$unifyFields = F6(
	function (baseVar, refs, oldExtends, oldFields, newExtends, newFields) {
		var extraOldFields = A2($elm$core$Dict$diff, oldFields, newFields);
		var extraNewFields = A2($elm$core$Dict$diff, newFields, oldFields);
		var commonFieldsOldType = A2($elm$core$Dict$intersect, oldFields, newFields);
		var fieldSolutionsResult = A2(
			$elm$core$Result$andThen,
			A2($author$project$Morphir$Type$Infer$concatSolutions, baseVar, refs),
			A2(
				$elm$core$Result$mapError,
				$author$project$Morphir$Type$Infer$typeErrors,
				$author$project$Morphir$ListOfResults$liftAllErrors(
					A2(
						$elm$core$List$map,
						function (_v8) {
							var fieldName = _v8.a;
							var originalType = _v8.b;
							return A2(
								$elm$core$Result$andThen,
								A4($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, _List_Nil, originalType),
								A2(
									$elm$core$Result$fromMaybe,
									$author$project$Morphir$Type$Infer$UnknownError(
										'Could not find field ' + $author$project$Morphir$IR$Name$toCamelCase(fieldName)),
									A2($elm$core$Dict$get, fieldName, newFields)));
						},
						$elm$core$Dict$toList(commonFieldsOldType)))));
		var unifiedFields = A2(
			$elm$core$Dict$union,
			commonFieldsOldType,
			A2($elm$core$Dict$union, extraOldFields, extraNewFields));
		return (_Utils_eq(oldExtends, $elm$core$Maybe$Nothing) && (!$elm$core$Dict$isEmpty(extraNewFields))) ? $elm$core$Result$Err(
			A3(
				$author$project$Morphir$Type$Infer$CouldNotUnify,
				3,
				A2($author$project$Morphir$Type$MetaType$MetaRecord, oldExtends, oldFields),
				A2($author$project$Morphir$Type$MetaType$MetaRecord, newExtends, newFields))) : ((_Utils_eq(newExtends, $elm$core$Maybe$Nothing) && (!$elm$core$Dict$isEmpty(extraOldFields))) ? $elm$core$Result$Err(
			A3(
				$author$project$Morphir$Type$Infer$CouldNotUnify,
				3,
				A2($author$project$Morphir$Type$MetaType$MetaRecord, oldExtends, oldFields),
				A2($author$project$Morphir$Type$MetaType$MetaRecord, newExtends, newFields))) : A2(
			$elm$core$Result$map,
			$elm$core$Tuple$pair(unifiedFields),
			fieldSolutionsResult));
	});
var $author$project$Morphir$Type$Infer$unifyFun = F6(
	function (baseVar, refs, aliases, arg1, return1, metaType2) {
		unifyFun:
		while (true) {
			switch (metaType2.$) {
				case 7:
					var alias = metaType2.a;
					var subject2 = metaType2.b;
					var $temp$baseVar = baseVar,
						$temp$refs = refs,
						$temp$aliases = A2($elm$core$List$cons, alias, aliases),
						$temp$arg1 = arg1,
						$temp$return1 = return1,
						$temp$metaType2 = subject2;
					baseVar = $temp$baseVar;
					refs = $temp$refs;
					aliases = $temp$aliases;
					arg1 = $temp$arg1;
					return1 = $temp$return1;
					metaType2 = $temp$metaType2;
					continue unifyFun;
				case 0:
					var var2 = metaType2.a;
					return A3(
						$author$project$Morphir$Type$Infer$unifyVariable,
						aliases,
						var2,
						A2($author$project$Morphir$Type$MetaType$MetaFun, arg1, return1));
				case 5:
					var arg2 = metaType2.a;
					var return2 = metaType2.b;
					return A2(
						$elm$core$Result$andThen,
						$elm$core$Basics$identity,
						A3(
							$elm$core$Result$map2,
							A2($author$project$Morphir$Type$Infer$mergeSolutions, baseVar, refs),
							A5($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, aliases, arg1, arg2),
							A5($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, aliases, return1, return2)));
				default:
					return $elm$core$Result$Err(
						A3(
							$author$project$Morphir$Type$Infer$CouldNotUnify,
							0,
							A2($author$project$Morphir$Type$MetaType$MetaFun, arg1, return1),
							metaType2));
			}
		}
	});
var $author$project$Morphir$Type$Infer$unifyMetaType = F5(
	function (baseVar, refs, aliases, metaType1, metaType2) {
		unifyMetaType:
		while (true) {
			if (_Utils_eq(metaType1, metaType2)) {
				return $elm$core$Result$Ok($author$project$Morphir$Type$SolutionMap$empty);
			} else {
				switch (metaType1.$) {
					case 0:
						var var1 = metaType1.a;
						return A3($author$project$Morphir$Type$Infer$unifyVariable, aliases, var1, metaType2);
					case 2:
						var elems1 = metaType1.a;
						return A5($author$project$Morphir$Type$Infer$unifyTuple, baseVar, refs, aliases, elems1, metaType2);
					case 1:
						var ref1 = metaType1.a;
						return A5($author$project$Morphir$Type$Infer$unifyRef, baseVar, refs, aliases, ref1, metaType2);
					case 4:
						var fun1 = metaType1.a;
						var arg1 = metaType1.b;
						return A6($author$project$Morphir$Type$Infer$unifyApply, baseVar, refs, aliases, fun1, arg1, metaType2);
					case 5:
						var arg1 = metaType1.a;
						var return1 = metaType1.b;
						return A6($author$project$Morphir$Type$Infer$unifyFun, baseVar, refs, aliases, arg1, return1, metaType2);
					case 3:
						var extends1 = metaType1.a;
						var fields1 = metaType1.b;
						return A6($author$project$Morphir$Type$Infer$unifyRecord, baseVar, refs, aliases, extends1, fields1, metaType2);
					case 6:
						return A2($author$project$Morphir$Type$Infer$unifyUnit, aliases, metaType2);
					default:
						var alias = metaType1.a;
						var subject1 = metaType1.b;
						var $temp$baseVar = baseVar,
							$temp$refs = refs,
							$temp$aliases = A2($elm$core$List$cons, alias, aliases),
							$temp$metaType1 = subject1,
							$temp$metaType2 = metaType2;
						baseVar = $temp$baseVar;
						refs = $temp$refs;
						aliases = $temp$aliases;
						metaType1 = $temp$metaType1;
						metaType2 = $temp$metaType2;
						continue unifyMetaType;
				}
			}
		}
	});
var $author$project$Morphir$Type$Infer$unifyRecord = F6(
	function (baseVar, refs, aliases, extends1, fields1, metaType2) {
		unifyRecord:
		while (true) {
			switch (metaType2.$) {
				case 7:
					var alias = metaType2.a;
					var subject2 = metaType2.b;
					var $temp$baseVar = baseVar,
						$temp$refs = refs,
						$temp$aliases = A2($elm$core$List$cons, alias, aliases),
						$temp$extends1 = extends1,
						$temp$fields1 = fields1,
						$temp$metaType2 = subject2;
					baseVar = $temp$baseVar;
					refs = $temp$refs;
					aliases = $temp$aliases;
					extends1 = $temp$extends1;
					fields1 = $temp$fields1;
					metaType2 = $temp$metaType2;
					continue unifyRecord;
				case 0:
					var var2 = metaType2.a;
					return A3(
						$author$project$Morphir$Type$Infer$unifyVariable,
						aliases,
						var2,
						A2($author$project$Morphir$Type$MetaType$MetaRecord, extends1, fields1));
				case 3:
					var extends2 = metaType2.a;
					var fields2 = metaType2.b;
					return A2(
						$elm$core$Result$andThen,
						function (_v3) {
							var newFields = _v3.a;
							var fieldSolutions = _v3.b;
							if (!extends1.$) {
								var extendsVar1 = extends1.a;
								return A4(
									$author$project$Morphir$Type$Infer$mergeSolutions,
									baseVar,
									refs,
									fieldSolutions,
									A2(
										$author$project$Morphir$Type$SolutionMap$singleton,
										extendsVar1,
										A2(
											$author$project$Morphir$Type$Infer$wrapInAliases,
											aliases,
											A2($author$project$Morphir$Type$MetaType$MetaRecord, extends2, newFields))));
							} else {
								if (!extends2.$) {
									var extendsVar2 = extends2.a;
									return A4(
										$author$project$Morphir$Type$Infer$mergeSolutions,
										baseVar,
										refs,
										fieldSolutions,
										A2(
											$author$project$Morphir$Type$SolutionMap$singleton,
											extendsVar2,
											A2(
												$author$project$Morphir$Type$Infer$wrapInAliases,
												aliases,
												A2($author$project$Morphir$Type$MetaType$MetaRecord, extends1, newFields))));
								} else {
									return $elm$core$Result$Ok(fieldSolutions);
								}
							}
						},
						A6($author$project$Morphir$Type$Infer$unifyFields, baseVar, refs, extends1, fields1, extends2, fields2));
				default:
					return $elm$core$Result$Err(
						A3(
							$author$project$Morphir$Type$Infer$CouldNotUnify,
							0,
							A2($author$project$Morphir$Type$MetaType$MetaRecord, extends1, fields1),
							metaType2));
			}
		}
	});
var $author$project$Morphir$Type$Infer$unifyRef = F5(
	function (baseVar, refs, aliases, ref1, metaType2) {
		unifyRef:
		while (true) {
			switch (metaType2.$) {
				case 7:
					var alias = metaType2.a;
					var subject2 = metaType2.b;
					var $temp$baseVar = baseVar,
						$temp$refs = refs,
						$temp$aliases = A2($elm$core$List$cons, alias, aliases),
						$temp$ref1 = ref1,
						$temp$metaType2 = subject2;
					baseVar = $temp$baseVar;
					refs = $temp$refs;
					aliases = $temp$aliases;
					ref1 = $temp$ref1;
					metaType2 = $temp$metaType2;
					continue unifyRef;
				case 0:
					var var2 = metaType2.a;
					return A3(
						$author$project$Morphir$Type$Infer$unifyVariable,
						aliases,
						var2,
						$author$project$Morphir$Type$MetaType$MetaRef(ref1));
				case 1:
					var ref2 = metaType2.a;
					return _Utils_eq(ref1, ref2) ? $elm$core$Result$Ok($author$project$Morphir$Type$SolutionMap$empty) : $elm$core$Result$Err(
						A3(
							$author$project$Morphir$Type$Infer$CouldNotUnify,
							2,
							$author$project$Morphir$Type$MetaType$MetaRef(ref1),
							metaType2));
				case 3:
					var extends2 = metaType2.a;
					var fields2 = metaType2.b;
					return A6(
						$author$project$Morphir$Type$Infer$unifyRecord,
						baseVar,
						refs,
						aliases,
						extends2,
						fields2,
						$author$project$Morphir$Type$MetaType$MetaRef(ref1));
				default:
					return $elm$core$Result$Err(
						A3(
							$author$project$Morphir$Type$Infer$CouldNotUnify,
							0,
							$author$project$Morphir$Type$MetaType$MetaRef(ref1),
							metaType2));
			}
		}
	});
var $author$project$Morphir$Type$Infer$unifyTuple = F5(
	function (baseVar, refs, aliases, elems1, metaType2) {
		switch (metaType2.$) {
			case 0:
				var var2 = metaType2.a;
				return A3(
					$author$project$Morphir$Type$Infer$unifyVariable,
					aliases,
					var2,
					$author$project$Morphir$Type$MetaType$MetaTuple(elems1));
			case 2:
				var elems2 = metaType2.a;
				return _Utils_eq(
					$elm$core$List$length(elems1),
					$elm$core$List$length(elems2)) ? A2(
					$elm$core$Result$andThen,
					A2($author$project$Morphir$Type$Infer$concatSolutions, baseVar, refs),
					A2(
						$elm$core$Result$mapError,
						$author$project$Morphir$Type$Infer$TypeErrors,
						$author$project$Morphir$ListOfResults$liftAllErrors(
							A3(
								$elm$core$List$map2,
								A3($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, aliases),
								elems1,
								elems2)))) : $elm$core$Result$Err(
					A3(
						$author$project$Morphir$Type$Infer$CouldNotUnify,
						1,
						$author$project$Morphir$Type$MetaType$MetaTuple(elems1),
						metaType2));
			default:
				return $elm$core$Result$Err(
					A3(
						$author$project$Morphir$Type$Infer$CouldNotUnify,
						0,
						$author$project$Morphir$Type$MetaType$MetaTuple(elems1),
						metaType2));
		}
	});
var $author$project$Morphir$Type$Infer$findSubstitution = F3(
	function (baseVar, refs, constraints) {
		findSubstitution:
		while (true) {
			if (!constraints.b) {
				return $elm$core$Result$Ok($elm$core$Maybe$Nothing);
			} else {
				var firstConstraint = constraints.a;
				var restOfConstraints = constraints.b;
				if (!firstConstraint.$) {
					var metaType1 = firstConstraint.a;
					var metaType2 = firstConstraint.b;
					return A2(
						$elm$core$Result$andThen,
						function (solutions) {
							return $author$project$Morphir$Type$SolutionMap$isEmpty(solutions) ? A3($author$project$Morphir$Type$Infer$findSubstitution, baseVar, refs, restOfConstraints) : $elm$core$Result$Ok(
								$elm$core$Maybe$Just(solutions));
						},
						A5($author$project$Morphir$Type$Infer$unifyMetaType, baseVar, refs, _List_Nil, metaType1, metaType2));
				} else {
					var $temp$baseVar = baseVar,
						$temp$refs = refs,
						$temp$constraints = restOfConstraints;
					baseVar = $temp$baseVar;
					refs = $temp$refs;
					constraints = $temp$constraints;
					continue findSubstitution;
				}
			}
		}
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Morphir$Type$Infer$removeTrivialConstraints = function (constraints) {
	return A2(
		$elm$core$List$filter,
		function (constraint) {
			if (!constraint.$) {
				var metaType1 = constraint.a;
				var metaType2 = constraint.b;
				return !_Utils_eq(metaType1, metaType2);
			} else {
				var metaType = constraint.a;
				if (!metaType.$) {
					return true;
				} else {
					return false;
				}
			}
		},
		constraints);
};
var $author$project$Morphir$Type$Infer$ClassConstraintViolation = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Morphir$Type$MetaType$intType = $author$project$Morphir$Type$MetaType$MetaRef(
	A3($author$project$Morphir$IR$FQName$fqn, 'Morphir.SDK', 'Basics', 'Int'));
var $author$project$Morphir$Type$Class$numberTypes = _List_fromArray(
	[$author$project$Morphir$Type$MetaType$intType, $author$project$Morphir$Type$MetaType$floatType]);
var $author$project$Morphir$Type$Class$member = F2(
	function (metaType, _class) {
		var targetType = function (mt) {
			targetType:
			while (true) {
				if (mt.$ === 7) {
					var t = mt.b;
					var $temp$mt = t;
					mt = $temp$mt;
					continue targetType;
				} else {
					return mt;
				}
			}
		};
		return A2(
			$elm$core$List$member,
			targetType(metaType),
			$author$project$Morphir$Type$Class$numberTypes);
	});
var $author$project$Morphir$Type$Infer$validateConstraints = function (constraints) {
	return A2(
		$elm$core$Result$mapError,
		$author$project$Morphir$Type$Infer$typeErrors,
		$author$project$Morphir$ListOfResults$liftAllErrors(
			A2(
				$elm$core$List$map,
				function (constraint) {
					if (constraint.$ === 1) {
						if (!constraint.a.$) {
							return $elm$core$Result$Ok(constraint);
						} else {
							var metaType = constraint.a;
							var _class = constraint.b;
							return A2($author$project$Morphir$Type$Class$member, metaType, _class) ? $elm$core$Result$Ok(constraint) : $elm$core$Result$Err(
								A2($author$project$Morphir$Type$Infer$ClassConstraintViolation, metaType, _class));
						}
					} else {
						return $elm$core$Result$Ok(constraint);
					}
				},
				constraints)));
};
var $author$project$Morphir$Type$Infer$solveHelp = F4(
	function (baseVar, refs, solutionsSoFar, constraintSet) {
		var constraints = constraintSet;
		return A2(
			$elm$core$Result$andThen,
			function (nonTrivialConstraints) {
				return A2(
					$elm$core$Result$andThen,
					function (maybeNewSolutions) {
						if (maybeNewSolutions.$ === 1) {
							return $elm$core$Result$Ok(
								_Utils_Tuple2(
									$author$project$Morphir$Type$ConstraintSet$fromList(nonTrivialConstraints),
									solutionsSoFar));
						} else {
							var newSolutions = maybeNewSolutions.a;
							return A2(
								$elm$core$Result$andThen,
								function (mergedSolutions) {
									return A4(
										$author$project$Morphir$Type$Infer$solveHelp,
										baseVar,
										refs,
										mergedSolutions,
										A2($author$project$Morphir$Type$ConstraintSet$applySubstitutions, mergedSolutions, constraintSet));
								},
								A4($author$project$Morphir$Type$Infer$mergeSolutions, baseVar, refs, newSolutions, solutionsSoFar));
						}
					},
					A3($author$project$Morphir$Type$Infer$findSubstitution, baseVar, refs, nonTrivialConstraints));
			},
			A2(
				$elm$core$Result$map,
				$author$project$Morphir$Type$Infer$removeTrivialConstraints,
				$author$project$Morphir$Type$Infer$validateConstraints(constraints)));
	});
var $author$project$Morphir$Type$Infer$solve = F3(
	function (baseVar, refs, constraintSet) {
		return A4($author$project$Morphir$Type$Infer$solveHelp, baseVar, refs, $author$project$Morphir$Type$SolutionMap$empty, constraintSet);
	});
var $author$project$Morphir$Type$Infer$inferValueDefinition = F2(
	function (refs, def) {
		var _v0 = A2($author$project$Morphir$Type$Infer$annotateDefinition, 0, def);
		var annotatedDef = _v0.a;
		var lastVarIndex = _v0.b;
		var constraints = A3(
			$author$project$Morphir$Type$Infer$constrainDefinition,
			$author$project$Morphir$Type$MetaType$variable(0),
			{ai: refs, z: $elm$core$Dict$empty},
			annotatedDef);
		var solution = A3(
			$author$project$Morphir$Type$Infer$solve,
			$author$project$Morphir$Type$MetaType$variable(lastVarIndex + 1),
			refs,
			constraints);
		return A2(
			$elm$core$Result$map,
			$author$project$Morphir$Type$Infer$applySolutionToAnnotatedDefinition(annotatedDef),
			solution);
	});
var $author$project$Morphir$IR$Path$toString = F3(
	function (nameToString, sep, path) {
		return A2(
			$elm$core$String$join,
			sep,
			A2(
				$elm$core$List$map,
				nameToString,
				$author$project$Morphir$IR$Path$toList(path)));
	});
var $author$project$Morphir$IR$Name$toTitleCase = function (name) {
	return A2(
		$elm$core$String$join,
		'',
		A2(
			$elm$core$List$map,
			$author$project$Morphir$IR$Name$capitalize,
			$author$project$Morphir$IR$Name$toList(name)));
};
var $author$project$Morphir$IR$FQName$toString = function (_v0) {
	var p = _v0.a;
	var m = _v0.b;
	var l = _v0.c;
	return A2(
		$elm$core$String$join,
		':',
		_List_fromArray(
			[
				A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', p),
				A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', m),
				$author$project$Morphir$IR$Name$toCamelCase(l)
			]));
};
var $author$project$Morphir$Type$Class$toString = function (_class) {
	return 'number';
};
var $author$project$Morphir$Type$MetaType$toString = function (metaType) {
	switch (metaType.$) {
		case 0:
			var _v1 = metaType.a;
			var i = _v1.a;
			var j = _v1.b;
			return 'var_' + ($elm$core$String$fromInt(i) + ('_' + $elm$core$String$fromInt(j)));
		case 1:
			var fQName = metaType.a;
			return $author$project$Morphir$IR$FQName$toString(fQName);
		case 2:
			var metaTypes = metaType.a;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'( ',
						A2(
						$elm$core$String$join,
						', ',
						A2($elm$core$List$map, $author$project$Morphir$Type$MetaType$toString, metaTypes)),
						' )'
					]));
		case 3:
			var _extends = metaType.a;
			var fields = metaType.b;
			var prefix = function () {
				if (!_extends.$) {
					var _v4 = _extends.a;
					var i = _v4.a;
					var j = _v4.b;
					return 'var_' + ($elm$core$String$fromInt(i) + ('_' + ($elm$core$String$fromInt(j) + ' | ')));
				} else {
					return '';
				}
			}();
			var fieldStrings = A2(
				$elm$core$List$map,
				function (_v2) {
					var fieldName = _v2.a;
					var fieldType = _v2.b;
					return $elm$core$String$concat(
						_List_fromArray(
							[
								$author$project$Morphir$IR$Name$toCamelCase(fieldName),
								' : ',
								$author$project$Morphir$Type$MetaType$toString(fieldType)
							]));
				},
				$elm$core$Dict$toList(fields));
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'{ ',
						prefix,
						A2($elm$core$String$join, ', ', fieldStrings),
						' }'
					]));
		case 4:
			var funType = metaType.a;
			var argType = metaType.b;
			if (argType.$ === 4) {
				return $elm$core$String$concat(
					_List_fromArray(
						[
							$author$project$Morphir$Type$MetaType$toString(funType),
							' (',
							$author$project$Morphir$Type$MetaType$toString(argType),
							')'
						]));
			} else {
				return $elm$core$String$concat(
					_List_fromArray(
						[
							$author$project$Morphir$Type$MetaType$toString(funType),
							' ',
							$author$project$Morphir$Type$MetaType$toString(argType)
						]));
			}
		case 5:
			var argType = metaType.a;
			var returnType = metaType.b;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						$author$project$Morphir$Type$MetaType$toString(argType),
						' -> ',
						$author$project$Morphir$Type$MetaType$toString(returnType)
					]));
		case 6:
			return '()';
		default:
			var alias = metaType.a;
			var targetType = metaType.b;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						$author$project$Morphir$Type$MetaType$toString(targetType),
						' as ',
						$author$project$Morphir$IR$FQName$toString(alias)
					]));
	}
};
var $author$project$Morphir$Type$Infer$typeErrorToMessage = function (typeError) {
	switch (typeError.$) {
		case 0:
			var errors = typeError.a;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Multiple errors: ',
						A2(
						$elm$core$String$join,
						', ',
						A2($elm$core$List$map, $author$project$Morphir$Type$Infer$typeErrorToMessage, errors))
					]));
		case 1:
			var metaType = typeError.a;
			var _class = typeError.b;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Type \'',
						$author$project$Morphir$Type$MetaType$toString(metaType),
						'\' is not a ',
						$author$project$Morphir$Type$Class$toString(_class)
					]));
		case 2:
			var lookupError = typeError.a;
			switch (lookupError.$) {
				case 0:
					var fQName = lookupError.a;
					return $elm$core$String$concat(
						_List_fromArray(
							[
								'Could not find constructor: ',
								$author$project$Morphir$IR$FQName$toString(fQName)
							]));
				case 1:
					var fQName = lookupError.a;
					return $elm$core$String$concat(
						_List_fromArray(
							[
								'Could not find value: ',
								$author$project$Morphir$IR$FQName$toString(fQName)
							]));
				case 2:
					var fQName = lookupError.a;
					return $elm$core$String$concat(
						_List_fromArray(
							[
								'Could not find alias: ',
								$author$project$Morphir$IR$FQName$toString(fQName)
							]));
				default:
					var fQName = lookupError.a;
					return $elm$core$String$concat(
						_List_fromArray(
							[
								'Expected alias at: ',
								$author$project$Morphir$IR$FQName$toString(fQName)
							]));
			}
		case 3:
			var message = typeError.a;
			return $elm$core$String$concat(
				_List_fromArray(
					['Unknown error: ', message]));
		default:
			var unificationError = typeError.a;
			var metaType1 = typeError.b;
			var metaType2 = typeError.c;
			var cause = function () {
				switch (unificationError) {
					case 0:
						return 'there are no unification rules to apply';
					case 1:
						return 'they are tuples of different sizes';
					case 2:
						return 'the references do not match';
					default:
						return 'the fields don\'t match';
				}
			}();
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Could not unify \'',
						$author$project$Morphir$Type$MetaType$toString(metaType1),
						'\' with \'',
						$author$project$Morphir$Type$MetaType$toString(metaType2),
						'\' because ',
						cause
					]));
	}
};
var $author$project$Morphir$Type$Infer$inferModuleDefinition = F3(
	function (refs, moduleName, moduleDef) {
		return A2(
			$elm$core$Result$mapError,
			$author$project$Morphir$Compiler$ErrorsInSourceFile(
				A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', moduleName)),
			A2(
				$elm$core$Result$map,
				function (mappedValues) {
					return {
						gn: moduleDef.gn,
						gq: $elm$core$Dict$fromList(mappedValues)
					};
				},
				$author$project$Morphir$ListOfResults$liftAllErrors(
					A2(
						$elm$core$List$map,
						function (_v0) {
							var valueName = _v0.a;
							var valueDef = _v0.b;
							return A2(
								$elm$core$Result$mapError,
								function (typeError) {
									return A2(
										$author$project$Morphir$Compiler$ErrorInSourceFile,
										$elm$core$String$concat(
											_List_fromArray(
												[
													'Type error in value \'',
													$author$project$Morphir$IR$Name$toCamelCase(valueName),
													'\': ',
													$author$project$Morphir$Type$Infer$typeErrorToMessage(typeError)
												])),
										_List_Nil);
								},
								A2(
									$elm$core$Result$map,
									$elm$core$Tuple$pair(valueName),
									A2(
										$elm$core$Result$map,
										$author$project$Morphir$IR$AccessControlled$AccessControlled(valueDef.b0),
										A2($author$project$Morphir$Type$Infer$inferValueDefinition, refs, valueDef.aT))));
						},
						$elm$core$Dict$toList(moduleDef.gq)))));
	});
var $author$project$Morphir$Type$Infer$inferPackageDefinition = F2(
	function (refs, packageDef) {
		return A2(
			$elm$core$Result$map,
			function (mappedModules) {
				return {
					e7: $elm$core$Dict$fromList(mappedModules)
				};
			},
			$author$project$Morphir$ListOfResults$liftAllErrors(
				A2(
					$elm$core$List$map,
					function (_v0) {
						var moduleName = _v0.a;
						var moduleDef = _v0.b;
						return A2(
							$elm$core$Result$map,
							$elm$core$Tuple$pair(moduleName),
							A2(
								$elm$core$Result$map,
								$author$project$Morphir$IR$AccessControlled$AccessControlled(moduleDef.b0),
								A3($author$project$Morphir$Type$Infer$inferModuleDefinition, refs, moduleName, moduleDef.aT)));
					},
					$elm$core$Dict$toList(packageDef.e7))));
	});
var $author$project$Morphir$Elm$CLI$jsonDecodeError = _Platform_outgoingPort('jsonDecodeError', $elm$json$Json$Encode$string);
var $author$project$Morphir$IR$Type$mapDefinitionAttributes = F2(
	function (f, def) {
		if (!def.$) {
			var params = def.a;
			var tpe = def.b;
			return A2(
				$author$project$Morphir$IR$Type$TypeAliasDefinition,
				params,
				A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, tpe));
		} else {
			var params = def.a;
			var constructors = def.b;
			return A2(
				$author$project$Morphir$IR$Type$CustomTypeDefinition,
				params,
				A2(
					$author$project$Morphir$IR$AccessControlled$AccessControlled,
					constructors.b0,
					A2(
						$elm$core$List$map,
						function (_v1) {
							var ctorName = _v1.a;
							var ctorArgs = _v1.b;
							return A2(
								$author$project$Morphir$IR$Type$Constructor,
								ctorName,
								A2(
									$elm$core$List$map,
									function (_v2) {
										var argName = _v2.a;
										var argType = _v2.b;
										return _Utils_Tuple2(
											argName,
											A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, argType));
									},
									ctorArgs));
						},
						constructors.aT)));
		}
	});
var $author$project$Morphir$IR$Module$mapDefinitionAttributes = F3(
	function (tf, vf, def) {
		return A2(
			$author$project$Morphir$IR$Module$Definition,
			A2(
				$elm$core$Dict$map,
				F2(
					function (_v0, typeDef) {
						return A2(
							$author$project$Morphir$IR$AccessControlled$AccessControlled,
							typeDef.b0,
							A2(
								$author$project$Morphir$IR$Documented$map,
								$author$project$Morphir$IR$Type$mapDefinitionAttributes(tf),
								typeDef.aT));
					}),
				def.gn),
			A2(
				$elm$core$Dict$map,
				F2(
					function (_v1, valueDef) {
						return A2(
							$author$project$Morphir$IR$AccessControlled$AccessControlled,
							valueDef.b0,
							A3($author$project$Morphir$IR$Value$mapDefinitionAttributes, tf, vf, valueDef.aT));
					}),
				def.gq));
	});
var $author$project$Morphir$IR$Package$mapDefinitionAttributes = F3(
	function (tf, vf, def) {
		return $author$project$Morphir$IR$Package$Definition(
			A2(
				$elm$core$Dict$map,
				F2(
					function (_v0, moduleDef) {
						return A2(
							$author$project$Morphir$IR$AccessControlled$AccessControlled,
							moduleDef.b0,
							A3($author$project$Morphir$IR$Module$mapDefinitionAttributes, tf, vf, moduleDef.aT));
					}),
				def.e7));
	});
var $author$project$Morphir$IR$FQName$getLocalName = function (_v0) {
	var l = _v0.c;
	return l;
};
var $author$project$Morphir$IR$FQName$getModulePath = function (_v0) {
	var m = _v0.b;
	return m;
};
var $author$project$Morphir$IR$FQName$getPackagePath = function (_v0) {
	var p = _v0.a;
	return p;
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$String$length = _String_length;
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Morphir$IR$Name$toHumanWords = function (name) {
	var words = $author$project$Morphir$IR$Name$toList(name);
	var join = function (abbrev) {
		return $elm$core$String$toUpper(
			A2($elm$core$String$join, '', abbrev));
	};
	var process = F3(
		function (prefix, abbrev, suffix) {
			process:
			while (true) {
				if (!suffix.b) {
					return $elm$core$List$isEmpty(abbrev) ? prefix : A2(
						$elm$core$List$append,
						prefix,
						_List_fromArray(
							[
								join(abbrev)
							]));
				} else {
					var first = suffix.a;
					var rest = suffix.b;
					if ($elm$core$String$length(first) === 1) {
						var $temp$prefix = prefix,
							$temp$abbrev = A2(
							$elm$core$List$append,
							abbrev,
							_List_fromArray(
								[first])),
							$temp$suffix = rest;
						prefix = $temp$prefix;
						abbrev = $temp$abbrev;
						suffix = $temp$suffix;
						continue process;
					} else {
						if (!abbrev.b) {
							var $temp$prefix = A2(
								$elm$core$List$append,
								prefix,
								_List_fromArray(
									[first])),
								$temp$abbrev = _List_Nil,
								$temp$suffix = rest;
							prefix = $temp$prefix;
							abbrev = $temp$abbrev;
							suffix = $temp$suffix;
							continue process;
						} else {
							var $temp$prefix = A2(
								$elm$core$List$append,
								prefix,
								_List_fromArray(
									[
										join(abbrev),
										first
									])),
								$temp$abbrev = _List_Nil,
								$temp$suffix = rest;
							prefix = $temp$prefix;
							abbrev = $temp$abbrev;
							suffix = $temp$suffix;
							continue process;
						}
					}
				}
			}
		});
	return A3(process, _List_Nil, _List_Nil, words);
};
var $author$project$Morphir$IR$Name$toSnakeCase = function (name) {
	return A2(
		$elm$core$String$join,
		'_',
		$author$project$Morphir$IR$Name$toHumanWords(name));
};
var $author$project$Morphir$Graph$Grapher$fqnToString = function (fqn) {
	return A2(
		$elm$core$String$join,
		'.',
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$Path$toString,
				$author$project$Morphir$IR$Name$toSnakeCase,
				'.',
				$author$project$Morphir$IR$FQName$getPackagePath(fqn)),
				A3(
				$author$project$Morphir$IR$Path$toString,
				$author$project$Morphir$IR$Name$toSnakeCase,
				'.',
				$author$project$Morphir$IR$FQName$getModulePath(fqn)),
				$author$project$Morphir$IR$Name$toSnakeCase(
				$author$project$Morphir$IR$FQName$getLocalName(fqn))
			]));
};
var $author$project$Morphir$Graph$Grapher$nodeType = function (node) {
	switch (node.$) {
		case 0:
			return 'Record';
		case 1:
			return 'Field';
		case 2:
			return 'Type';
		case 3:
			return 'Function';
		default:
			return 'Unknown';
	}
};
var $author$project$Morphir$Graph$Grapher$verbToString = function (verb) {
	switch (verb) {
		case 0:
			return 'isA';
		case 1:
			return 'aliases';
		case 2:
			return 'contains';
		case 3:
			return 'uses';
		case 4:
			return 'calls';
		case 5:
			return 'produces';
		case 6:
			return 'parameterizes';
		default:
			return 'unions';
	}
};
var $author$project$Morphir$Graph$Grapher$graphEntryToComparable = function (entry) {
	var nodeId = function (node) {
		return $author$project$Morphir$Graph$Grapher$nodeType(node) + (':' + function () {
			switch (node.$) {
				case 0:
					var fqn = node.a;
					return $author$project$Morphir$Graph$Grapher$fqnToString(fqn);
				case 1:
					var fqn = node.a;
					var name = node.b;
					return $author$project$Morphir$Graph$Grapher$fqnToString(fqn) + ('#' + $author$project$Morphir$IR$Name$toSnakeCase(name));
				case 2:
					var fqn = node.a;
					return $author$project$Morphir$Graph$Grapher$fqnToString(fqn);
				case 3:
					var fqn = node.a;
					return $author$project$Morphir$Graph$Grapher$fqnToString(fqn);
				default:
					var s = node.a;
					return 'unknown:' + s;
			}
		}());
	};
	var edgeToString = function (edge) {
		return nodeId(edge.fZ) + (' ' + ($author$project$Morphir$Graph$Grapher$verbToString(edge.gr) + (' ' + nodeId(edge.fi))));
	};
	if (!entry.$) {
		var node = entry.a;
		return 'NodeEntry: ' + nodeId(node);
	} else {
		var edge = entry.a;
		return 'EdgeEntry: ' + edgeToString(edge);
	}
};
var $author$project$Morphir$Graph$Grapher$asNode = function (entry) {
	if (!entry.$) {
		var node = entry.a;
		return $elm$core$Maybe$Just(node);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Morphir$Graph$Grapher$Aliases = 1;
var $author$project$Morphir$Graph$Grapher$Contains = 2;
var $author$project$Morphir$Graph$Grapher$Edge = F3(
	function (subject, verb, object) {
		return {fi: object, fZ: subject, gr: verb};
	});
var $author$project$Morphir$Graph$Grapher$EdgeEntry = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Graph$Grapher$Field = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Morphir$Graph$Grapher$IsA = 0;
var $author$project$Morphir$Graph$Grapher$NodeEntry = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Graph$Grapher$Record = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Graph$Grapher$Type = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Graph$Grapher$Unions = 7;
var $author$project$Morphir$Graph$Grapher$Unknown = function (a) {
	return {$: 4, a: a};
};
var $author$project$Morphir$Graph$Grapher$leafType = function (tipe) {
	if (tipe.$ === 1) {
		var tipeFQN = tipe.b;
		var paramTypes = tipe.c;
		if (!paramTypes.b) {
			return A2($elm$core$List$cons, tipeFQN, _List_Nil);
		} else {
			return A2($elm$core$List$concatMap, $author$project$Morphir$Graph$Grapher$leafType, paramTypes);
		}
	} else {
		return _List_Nil;
	}
};
var $author$project$Morphir$Graph$Grapher$mapTypeDefinition = F4(
	function (packageName, moduleName, typeName, typeDef) {
		var fqn = A3($author$project$Morphir$IR$FQName$FQName, packageName, moduleName, typeName);
		var triples = function () {
			if (!typeDef.$) {
				switch (typeDef.b.$) {
					case 3:
						var _v1 = typeDef.b;
						var fields = _v1.b;
						var recordNode = $author$project$Morphir$Graph$Grapher$Record(fqn);
						var fieldTriples = A2(
							$elm$core$List$map,
							function (field) {
								var fieldType = function () {
									var _v2 = field.gl;
									if (_v2.$ === 1) {
										var typeFqn = _v2.b;
										return $author$project$Morphir$Graph$Grapher$Type(typeFqn);
									} else {
										return $author$project$Morphir$Graph$Grapher$Unknown('Alias');
									}
								}();
								var fieldNode = A2(
									$author$project$Morphir$Graph$Grapher$Field,
									A3($author$project$Morphir$IR$FQName$FQName, packageName, moduleName, typeName),
									field.cN);
								return _List_fromArray(
									[
										$author$project$Morphir$Graph$Grapher$NodeEntry(fieldNode),
										$author$project$Morphir$Graph$Grapher$NodeEntry(fieldType),
										$author$project$Morphir$Graph$Grapher$EdgeEntry(
										A3($author$project$Morphir$Graph$Grapher$Edge, recordNode, 2, fieldNode)),
										$author$project$Morphir$Graph$Grapher$EdgeEntry(
										A3($author$project$Morphir$Graph$Grapher$Edge, fieldNode, 0, fieldType))
									]);
							},
							fields);
						return A2(
							$elm$core$List$cons,
							$author$project$Morphir$Graph$Grapher$NodeEntry(recordNode),
							$elm$core$List$concat(fieldTriples));
					case 1:
						var _v3 = typeDef.b;
						var aliasFQN = _v3.b;
						var thisNode = $author$project$Morphir$Graph$Grapher$Type(fqn);
						var aliasNode = $author$project$Morphir$Graph$Grapher$Type(aliasFQN);
						return _List_fromArray(
							[
								$author$project$Morphir$Graph$Grapher$NodeEntry(thisNode),
								$author$project$Morphir$Graph$Grapher$NodeEntry(aliasNode),
								$author$project$Morphir$Graph$Grapher$EdgeEntry(
								A3($author$project$Morphir$Graph$Grapher$Edge, thisNode, 1, aliasNode))
							]);
					default:
						return _List_Nil;
				}
			} else {
				var accessControlledCtors = typeDef.b;
				var typeNode = $author$project$Morphir$Graph$Grapher$Type(fqn);
				var leafEntries = function () {
					var _v4 = $author$project$Morphir$IR$AccessControlled$withPublicAccess(accessControlledCtors);
					if (!_v4.$) {
						var ctors = _v4.a;
						return $elm$core$List$concat(
							A2(
								$elm$core$List$map,
								function (constructor) {
									var namesAndTypes = constructor.b;
									return A2(
										$elm$core$List$concatMap,
										function (_v6) {
											var tipe = _v6.b;
											return A2(
												$elm$core$List$concatMap,
												function (leafFqn) {
													var leafNode = $author$project$Morphir$Graph$Grapher$Type(leafFqn);
													return _List_fromArray(
														[
															$author$project$Morphir$Graph$Grapher$NodeEntry(leafNode),
															$author$project$Morphir$Graph$Grapher$EdgeEntry(
															A3($author$project$Morphir$Graph$Grapher$Edge, typeNode, 7, leafNode))
														]);
												},
												$author$project$Morphir$Graph$Grapher$leafType(tipe));
										},
										namesAndTypes);
								},
								ctors));
					} else {
						return _List_Nil;
					}
				}();
				return A2(
					$elm$core$List$cons,
					$author$project$Morphir$Graph$Grapher$NodeEntry(typeNode),
					leafEntries);
			}
		}();
		return triples;
	});
var $author$project$Morphir$Graph$Grapher$mapModuleTypes = F3(
	function (packageName, moduleName, moduleDef) {
		return A2(
			$elm$core$List$concatMap,
			function (_v0) {
				var typeName = _v0.a;
				var accessControlledDocumentedTypeDef = _v0.b;
				return A4($author$project$Morphir$Graph$Grapher$mapTypeDefinition, packageName, moduleName, typeName, accessControlledDocumentedTypeDef.aT.aT);
			},
			$elm$core$Dict$toList(moduleDef.gn));
	});
var $author$project$Morphir$Graph$Grapher$Calls = 4;
var $author$project$Morphir$Graph$Grapher$Function = function (a) {
	return {$: 3, a: a};
};
var $author$project$Morphir$Graph$Grapher$Produces = 5;
var $author$project$Morphir$Graph$Grapher$Uses = 3;
var $author$project$Morphir$Graph$Grapher$collectReferences = F2(
	function (referenceFQN, children) {
		if (!children.b) {
			return A2($elm$core$List$cons, referenceFQN, _List_Nil);
		} else {
			return A2(
				$elm$core$List$concatMap,
				function (child) {
					if (child.$ === 1) {
						var childFQN = child.b;
						var grandChildren = child.c;
						return A2($author$project$Morphir$Graph$Grapher$collectReferences, childFQN, grandChildren);
					} else {
						return _List_Nil;
					}
				},
				children);
		}
	});
var $author$project$Morphir$Graph$Grapher$mapValueDefinition = F5(
	function (packageName, moduleName, valueName, valueDef, nodeRegistry) {
		var lookupNode = function (key) {
			return A2($elm$core$Dict$get, key, nodeRegistry);
		};
		var makeRefEdge = F4(
			function (subject, verb, key, _default) {
				var node = lookupNode(
					$author$project$Morphir$Graph$Grapher$fqnToString(key));
				return A2(
					$elm$core$Maybe$withDefault,
					_List_fromArray(
						[
							$author$project$Morphir$Graph$Grapher$NodeEntry(_default),
							$author$project$Morphir$Graph$Grapher$EdgeEntry(
							A3($author$project$Morphir$Graph$Grapher$Edge, subject, verb, _default))
						]),
					A2(
						$elm$core$Maybe$map,
						function (object) {
							return _List_fromArray(
								[
									$author$project$Morphir$Graph$Grapher$EdgeEntry(
									A3($author$project$Morphir$Graph$Grapher$Edge, subject, verb, object))
								]);
						},
						node));
			});
		var functionFqn = A3($author$project$Morphir$IR$FQName$FQName, packageName, moduleName, valueName);
		var functionNode = $author$project$Morphir$Graph$Grapher$Function(functionFqn);
		var inputTriples = A2(
			$elm$core$List$concatMap,
			function (fqn) {
				return A4(
					makeRefEdge,
					functionNode,
					3,
					fqn,
					$author$project$Morphir$Graph$Grapher$Type(fqn));
			},
			A2(
				$elm$core$List$concatMap,
				function (inputType) {
					if (inputType.c.$ === 1) {
						var _v5 = inputType.c;
						var inputFqn = _v5.b;
						var children = _v5.c;
						return A2($author$project$Morphir$Graph$Grapher$collectReferences, inputFqn, children);
					} else {
						return _List_Nil;
					}
				},
				valueDef.a3));
		var outputTriples = function () {
			var _v3 = valueDef.fq;
			switch (_v3.$) {
				case 1:
					var outputFQN = _v3.b;
					return A4(
						makeRefEdge,
						functionNode,
						5,
						outputFQN,
						$author$project$Morphir$Graph$Grapher$Type(outputFQN));
				case 2:
					var tupleTypes = _v3.b;
					return A2(
						$elm$core$List$concatMap,
						function (leafFQN) {
							return A4(
								makeRefEdge,
								functionNode,
								5,
								leafFQN,
								$author$project$Morphir$Graph$Grapher$Type(leafFQN));
						},
						A2($elm$core$List$concatMap, $author$project$Morphir$Graph$Grapher$leafType, tupleTypes));
				default:
					return _List_Nil;
			}
		}();
		var subFunctionTriples = function () {
			var collectFunctions = function (value) {
				collectFunctions:
				while (true) {
					switch (value.$) {
						case 6:
							var calledFQN = value.b;
							return A4(
								makeRefEdge,
								functionNode,
								4,
								calledFQN,
								$author$project$Morphir$Graph$Grapher$Function(calledFQN));
						case 2:
							var values = value.b;
							return A2($elm$core$List$concatMap, collectFunctions, values);
						case 3:
							var values = value.b;
							return A2($elm$core$List$concatMap, collectFunctions, values);
						case 7:
							var v = value.b;
							var name = value.c;
							var $temp$value = v;
							value = $temp$value;
							continue collectFunctions;
						case 9:
							var value1 = value.b;
							var value2 = value.c;
							return A2(
								$elm$core$List$concatMap,
								collectFunctions,
								_List_fromArray(
									[value1, value2]));
						case 10:
							var v = value.c;
							var $temp$value = v;
							value = $temp$value;
							continue collectFunctions;
						case 11:
							var v = value.d;
							var $temp$value = v;
							value = $temp$value;
							continue collectFunctions;
						case 12:
							var v = value.c;
							var $temp$value = v;
							value = $temp$value;
							continue collectFunctions;
						case 13:
							var value1 = value.c;
							var value2 = value.d;
							return A2(
								$elm$core$List$concatMap,
								collectFunctions,
								_List_fromArray(
									[value1, value2]));
						case 14:
							var value1 = value.b;
							var value2 = value.c;
							var value3 = value.d;
							return A2(
								$elm$core$List$concatMap,
								collectFunctions,
								_List_fromArray(
									[value1, value2, value3]));
						case 15:
							var v = value.b;
							var tuples = value.c;
							return _Utils_ap(
								collectFunctions(v),
								A2(
									$elm$core$List$concatMap,
									collectFunctions,
									A2(
										$elm$core$List$map,
										function (_v1) {
											var tk = _v1.a;
											var tv = _v1.b;
											return tv;
										},
										tuples)));
						case 16:
							var v = value.b;
							var tuples = value.c;
							return _Utils_ap(
								collectFunctions(v),
								A2(
									$elm$core$List$concatMap,
									collectFunctions,
									A2(
										$elm$core$List$map,
										function (_v2) {
											var tk = _v2.a;
											var tv = _v2.b;
											return tv;
										},
										tuples)));
						default:
							return _List_Nil;
					}
				}
			};
			return collectFunctions(valueDef.dV);
		}();
		return A2(
			$elm$core$List$cons,
			$author$project$Morphir$Graph$Grapher$NodeEntry(functionNode),
			_Utils_ap(
				subFunctionTriples,
				_Utils_ap(inputTriples, outputTriples)));
	});
var $author$project$Morphir$Graph$Grapher$mapModuleValues = F4(
	function (packageName, moduleName, moduleDef, typeRegistry) {
		return A2(
			$elm$core$List$concatMap,
			function (_v0) {
				var valueName = _v0.a;
				var accessControlledDocumentedValueDef = _v0.b;
				return A5($author$project$Morphir$Graph$Grapher$mapValueDefinition, packageName, moduleName, valueName, accessControlledDocumentedValueDef.aT, typeRegistry);
			},
			$elm$core$Dict$toList(moduleDef.gq));
	});
var $author$project$Morphir$Graph$Grapher$fieldToKey = F2(
	function (fqn, name) {
		return $author$project$Morphir$Graph$Grapher$fqnToString(fqn) + ('#' + $author$project$Morphir$IR$Name$toSnakeCase(name));
	});
var $author$project$Morphir$Graph$Grapher$nodeFQN = function (node) {
	switch (node.$) {
		case 0:
			var fqn = node.a;
			return fqn;
		case 1:
			var fqn = node.a;
			return fqn;
		case 2:
			var fqn = node.a;
			return fqn;
		case 3:
			var fqn = node.a;
			return fqn;
		default:
			var s = node.a;
			return A3(
				$author$project$Morphir$IR$FQName$FQName,
				_List_Nil,
				_List_Nil,
				_List_fromArray(
					[s]));
	}
};
var $author$project$Morphir$Graph$Grapher$referenceToKey = $author$project$Morphir$Graph$Grapher$fqnToString;
var $author$project$Morphir$Graph$Grapher$nodeToKey = function (node) {
	if (node.$ === 1) {
		var fqn = node.a;
		var name = node.b;
		return A2($author$project$Morphir$Graph$Grapher$fieldToKey, fqn, name);
	} else {
		return $author$project$Morphir$Graph$Grapher$referenceToKey(
			$author$project$Morphir$Graph$Grapher$nodeFQN(node));
	}
};
var $author$project$Morphir$Graph$Grapher$mapPackageDefinition = F2(
	function (packageName, packageDef) {
		var types = A2(
			$elm$core$List$concatMap,
			function (_v1) {
				var moduleName = _v1.a;
				var accessControlledModuleDef = _v1.b;
				return A3($author$project$Morphir$Graph$Grapher$mapModuleTypes, packageName, moduleName, accessControlledModuleDef.aT);
			},
			$elm$core$Dict$toList(packageDef.e7));
		var typeRegistry = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (node) {
					return _Utils_Tuple2(
						$author$project$Morphir$Graph$Grapher$nodeToKey(node),
						node);
				},
				A2($elm$core$List$filterMap, $author$project$Morphir$Graph$Grapher$asNode, types)));
		var values = A2(
			$elm$core$List$concatMap,
			function (_v0) {
				var moduleName = _v0.a;
				var accessControlledModuleDef = _v0.b;
				return A4($author$project$Morphir$Graph$Grapher$mapModuleValues, packageName, moduleName, accessControlledModuleDef.aT, typeRegistry);
			},
			$elm$core$Dict$toList(packageDef.e7));
		return _Utils_ap(values, types);
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm_community$list_extra$List$Extra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			if (!remaining.b) {
				return $elm$core$List$reverse(accumulator);
			} else {
				var first = remaining.a;
				var rest = remaining.b;
				var computedFirst = f(first);
				if (A2($elm$core$Set$member, computedFirst, existing)) {
					var $temp$f = f,
						$temp$existing = existing,
						$temp$remaining = rest,
						$temp$accumulator = accumulator;
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				} else {
					var $temp$f = f,
						$temp$existing = A2($elm$core$Set$insert, computedFirst, existing),
						$temp$remaining = rest,
						$temp$accumulator = A2($elm$core$List$cons, first, accumulator);
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$uniqueBy = F2(
	function (f, list) {
		return A4($elm_community$list_extra$List$Extra$uniqueHelp, f, $elm$core$Set$empty, list, _List_Nil);
	});
var $author$project$Morphir$Graph$Grapher$mapDistribution = function (distro) {
	var packageName = distro.a;
	var packageDef = distro.c;
	return A2(
		$elm_community$list_extra$List$Extra$uniqueBy,
		$author$project$Morphir$Graph$Grapher$graphEntryToComparable,
		A2($author$project$Morphir$Graph$Grapher$mapPackageDefinition, packageName, packageDef));
};
var $author$project$Morphir$Graph$CypherBackend$fqnToString = function (fqn) {
	return A2(
		$elm$core$String$join,
		'.',
		_List_fromArray(
			[
				A3(
				$author$project$Morphir$IR$Path$toString,
				$author$project$Morphir$IR$Name$toSnakeCase,
				'.',
				$author$project$Morphir$IR$FQName$getPackagePath(fqn)),
				A3(
				$author$project$Morphir$IR$Path$toString,
				$author$project$Morphir$IR$Name$toSnakeCase,
				'.',
				$author$project$Morphir$IR$FQName$getModulePath(fqn)),
				$author$project$Morphir$IR$Name$toSnakeCase(
				$author$project$Morphir$IR$FQName$getLocalName(fqn))
			]));
};
var $author$project$Morphir$Graph$CypherBackend$splitNode = function (node) {
	var humanize = function (name) {
		return A2(
			$elm$core$String$join,
			' ',
			$author$project$Morphir$IR$Name$toHumanWords(name));
	};
	switch (node.$) {
		case 0:
			var fqn = node.a;
			return _Utils_Tuple3(
				$author$project$Morphir$Graph$Grapher$nodeType(node),
				$author$project$Morphir$Graph$CypherBackend$fqnToString(fqn),
				humanize(
					$author$project$Morphir$IR$FQName$getLocalName(fqn)));
		case 1:
			var fqn = node.a;
			var name = node.b;
			return _Utils_Tuple3(
				$author$project$Morphir$Graph$Grapher$nodeType(node),
				$author$project$Morphir$Graph$CypherBackend$fqnToString(fqn) + ('#' + $author$project$Morphir$IR$Name$toSnakeCase(name)),
				humanize(name));
		case 2:
			var fqn = node.a;
			return _Utils_Tuple3(
				$author$project$Morphir$Graph$Grapher$nodeType(node),
				$author$project$Morphir$Graph$CypherBackend$fqnToString(fqn),
				humanize(
					$author$project$Morphir$IR$FQName$getLocalName(fqn)));
		case 3:
			var fqn = node.a;
			return _Utils_Tuple3(
				$author$project$Morphir$Graph$Grapher$nodeType(node),
				$author$project$Morphir$Graph$CypherBackend$fqnToString(fqn),
				humanize(
					$author$project$Morphir$IR$FQName$getLocalName(fqn)));
		default:
			var s = node.a;
			return _Utils_Tuple3('Unknown', 'unknown', s);
	}
};
var $author$project$Morphir$Graph$CypherBackend$nodeToCreate = function (node) {
	var _v0 = $author$project$Morphir$Graph$CypherBackend$splitNode(node);
	var tipe = _v0.a;
	var id = _v0.b;
	var name = _v0.c;
	return 'CREATE (n:' + (tipe + (' {id:\'' + (id + ('\', name:\'' + (name + '\'});')))));
};
var $author$project$Morphir$Graph$CypherBackend$toRelationship = function (edge) {
	var create = 'CREATE (s)-[:' + ($author$project$Morphir$Graph$Grapher$verbToString(edge.gr) + ']->(o)');
	var _v0 = $author$project$Morphir$Graph$CypherBackend$splitNode(edge.fZ);
	var subjectType = _v0.a;
	var subjectId = _v0.b;
	var matchs = 'MATCH (s:' + (subjectType + (' {id:\'' + (subjectId + '\'})')));
	var _v1 = $author$project$Morphir$Graph$CypherBackend$splitNode(edge.fi);
	var objectType = _v1.a;
	var objectId = _v1.b;
	var matcho = 'MATCH (o:' + (objectType + (' {id:\'' + (objectId + '\'})')));
	return matchs + (' ' + (matcho + (' ' + (create + ';'))));
};
var $elm_community$list_extra$List$Extra$unique = function (list) {
	return A4($elm_community$list_extra$List$Extra$uniqueHelp, $elm$core$Basics$identity, $elm$core$Set$empty, list, _List_Nil);
};
var $author$project$Morphir$Graph$CypherBackend$mapDistribution = F2(
	function (opt, distro) {
		var graphEntries = $author$project$Morphir$Graph$Grapher$mapDistribution(distro);
		var nodes = A2(
			$elm$core$List$filterMap,
			function (entry) {
				if (!entry.$) {
					var node = entry.a;
					return $elm$core$Maybe$Just(
						$author$project$Morphir$Graph$CypherBackend$nodeToCreate(node));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			graphEntries);
		var edges = A2(
			$elm$core$List$filterMap,
			function (entry) {
				if (entry.$ === 1) {
					var edge = entry.a;
					return $elm$core$Maybe$Just(
						$author$project$Morphir$Graph$CypherBackend$toRelationship(edge));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			graphEntries);
		var content = A2(
			$elm$core$String$join,
			'\n',
			$elm_community$list_extra$List$Extra$unique(
				$elm$core$List$concat(
					_List_fromArray(
						[nodes, edges]))));
		return $elm$core$Dict$fromList(
			_List_fromArray(
				[
					_Utils_Tuple2(
					_Utils_Tuple2(
						_List_fromArray(
							['dist']),
						'graph.cypher'),
					content)
				]));
	});
var $author$project$Morphir$Graph$TriplesBackend$mapDistribution = F2(
	function (opt, distro) {
		return $elm$core$Dict$empty;
	});
var $author$project$Morphir$Scala$PrettyPrinter$Options = F2(
	function (indentDepth, maxWidth) {
		return {D: indentDepth, e0: maxWidth};
	});
var $author$project$Morphir$File$SourceCode$concat = $elm$core$String$concat;
var $author$project$Morphir$File$SourceCode$dot = '.';
var $author$project$Morphir$File$SourceCode$dotSep = function (parts) {
	return A2($elm$core$String$join, $author$project$Morphir$File$SourceCode$dot, parts);
};
var $author$project$Morphir$File$SourceCode$newLine = '\n';
var $author$project$Morphir$Scala$PrettyPrinter$mapAnnotated = F2(
	function (valueToDoc, annotated) {
		var _v0 = annotated.dN;
		if (!_v0.b) {
			return valueToDoc(annotated.aT);
		} else {
			var annotations = _v0;
			return $author$project$Morphir$File$SourceCode$concat(
				_List_fromArray(
					[
						$author$project$Morphir$File$SourceCode$dotSep(annotations),
						$author$project$Morphir$File$SourceCode$newLine,
						valueToDoc(annotated.aT),
						$author$project$Morphir$File$SourceCode$newLine
					]));
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapDocumented = F2(
	function (valueToDoc, documented) {
		var _v0 = documented.en;
		if (!_v0.$) {
			var doc = _v0.a;
			return $author$project$Morphir$File$SourceCode$concat(
				_List_fromArray(
					[
						$author$project$Morphir$File$SourceCode$concat(
						_List_fromArray(
							['/** ', doc, $author$project$Morphir$File$SourceCode$newLine])),
						$author$project$Morphir$File$SourceCode$concat(
						_List_fromArray(
							['*/', $author$project$Morphir$File$SourceCode$newLine])),
						valueToDoc(documented.aT)
					]));
		} else {
			return valueToDoc(documented.aT);
		}
	});
var $author$project$Morphir$File$SourceCode$empty = '';
var $elm$core$String$append = _String_append;
var $elm$core$String$lines = _String_lines;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $author$project$Morphir$File$SourceCode$space = ' ';
var $author$project$Morphir$File$SourceCode$indentLines = F2(
	function (depth, lines) {
		return A2(
			$elm$core$String$join,
			$author$project$Morphir$File$SourceCode$newLine,
			A2(
				$elm$core$List$map,
				function (line) {
					return A2(
						$elm$core$String$append,
						A2($elm$core$String$repeat, depth, $author$project$Morphir$File$SourceCode$space),
						line);
				},
				A2($elm$core$List$concatMap, $elm$core$String$lines, lines)));
	});
var $author$project$Morphir$File$SourceCode$indent = F2(
	function (depth, string) {
		return A2(
			$author$project$Morphir$File$SourceCode$indentLines,
			depth,
			$elm$core$String$lines(string));
	});
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Morphir$Scala$PrettyPrinter$mapLit = function (lit) {
	switch (lit.$) {
		case 0:
			var bool = lit.a;
			return bool ? 'true' : 'false';
		case 1:
			var _char = lit.a;
			return '\'' + ($elm$core$String$fromChar(_char) + '\'');
		case 2:
			var string = lit.a;
			return '\"' + (string + '\"');
		case 3:
			var _int = lit.a;
			return $elm$core$String$fromInt(_int);
		default:
			var _float = lit.a;
			return $elm$core$String$fromFloat(_float);
	}
};
var $author$project$Morphir$Scala$PrettyPrinter$mapModifier = function (mod) {
	switch (mod.$) {
		case 0:
			return 'sealed';
		case 1:
			return 'final';
		case 2:
			return 'case';
		case 3:
			return 'val';
		case 4:
			return 'package';
		case 5:
			return 'implicit';
		case 7:
			return 'abstract';
		default:
			var maybeScope = mod.a;
			if (!maybeScope.$) {
				var scope = maybeScope.a;
				return $author$project$Morphir$File$SourceCode$concat(
					_List_fromArray(
						['private[', scope, ']']));
			} else {
				return 'private';
			}
	}
};
var $author$project$Morphir$Scala$PrettyPrinter$mapModifiers = function (mods) {
	if (!mods.b) {
		return $author$project$Morphir$File$SourceCode$empty;
	} else {
		return _Utils_ap(
			A2(
				$elm$core$String$join,
				$author$project$Morphir$File$SourceCode$space,
				A2($elm$core$List$map, $author$project$Morphir$Scala$PrettyPrinter$mapModifier, mods)),
			$author$project$Morphir$File$SourceCode$space);
	}
};
var $author$project$Morphir$File$SourceCode$parens = function (string) {
	return $elm$core$String$concat(
		_List_fromArray(
			['(', string, ')']));
};
var $author$project$Morphir$Scala$PrettyPrinter$mapPattern = function (pattern) {
	switch (pattern.$) {
		case 0:
			return '_';
		case 1:
			var name = pattern.a;
			return name;
		case 2:
			var name = pattern.a;
			var aliasedPattern = pattern.b;
			return $author$project$Morphir$File$SourceCode$concat(
				_List_fromArray(
					[
						name,
						' @ ',
						$author$project$Morphir$Scala$PrettyPrinter$mapPattern(aliasedPattern)
					]));
		case 3:
			var lit = pattern.a;
			return $author$project$Morphir$Scala$PrettyPrinter$mapLit(lit);
		case 4:
			var path = pattern.a;
			var name = pattern.b;
			var argPatterns = pattern.c;
			var argsDoc = function () {
				if (!argPatterns.b) {
					return $author$project$Morphir$File$SourceCode$empty;
				} else {
					return $author$project$Morphir$File$SourceCode$parens(
						$author$project$Morphir$File$SourceCode$concat(
							A2(
								$elm$core$List$intersperse,
								', ',
								A2($elm$core$List$map, $author$project$Morphir$Scala$PrettyPrinter$mapPattern, argPatterns))));
				}
			}();
			return _Utils_ap(
				$author$project$Morphir$File$SourceCode$dotSep(
					_Utils_ap(
						path,
						_List_fromArray(
							[name]))),
				argsDoc);
		case 5:
			var elemPatterns = pattern.a;
			return $author$project$Morphir$File$SourceCode$parens(
				$author$project$Morphir$File$SourceCode$concat(
					A2(
						$elm$core$List$intersperse,
						', ',
						A2($elm$core$List$map, $author$project$Morphir$Scala$PrettyPrinter$mapPattern, elemPatterns))));
		case 6:
			return 'Nil';
		case 7:
			var headPattern = pattern.a;
			var tailPattern = pattern.b;
			return $author$project$Morphir$Scala$PrettyPrinter$mapPattern(headPattern) + (' :: ' + $author$project$Morphir$Scala$PrettyPrinter$mapPattern(tailPattern));
		default:
			var childPattern = pattern.a;
			var message = pattern.b;
			return $author$project$Morphir$Scala$PrettyPrinter$mapPattern(childPattern) + (' /* ' + (message + ' */ '));
	}
};
var $author$project$Morphir$Scala$PrettyPrinter$matchBlock = F2(
	function (opt, statements) {
		return $author$project$Morphir$File$SourceCode$concat(
			_List_fromArray(
				[
					'{',
					$author$project$Morphir$File$SourceCode$newLine,
					A2(
					$author$project$Morphir$File$SourceCode$indentLines,
					opt.D,
					A2(
						$elm$core$List$map,
						function (_v0) {
							var pattern = _v0.a;
							var value = _v0.b;
							return $author$project$Morphir$File$SourceCode$concat(
								_List_fromArray(
									[
										$author$project$Morphir$File$SourceCode$concat(
										_List_fromArray(
											['case ', pattern, ' => ', $author$project$Morphir$File$SourceCode$newLine])),
										A2($author$project$Morphir$File$SourceCode$indent, opt.D, value)
									]));
						},
						statements)),
					$author$project$Morphir$File$SourceCode$newLine,
					'}'
				]));
	});
var $author$project$Morphir$Scala$PrettyPrinter$statementBlock = F2(
	function (opt, statements) {
		return $author$project$Morphir$File$SourceCode$concat(
			_List_fromArray(
				[
					'{',
					$author$project$Morphir$File$SourceCode$newLine,
					A2($author$project$Morphir$File$SourceCode$indentLines, opt.D, statements),
					$author$project$Morphir$File$SourceCode$newLine,
					'}'
				]));
	});
var $author$project$Morphir$Scala$PrettyPrinter$argValueBlock = F2(
	function (opt, argValues) {
		var mapArgValue = function (_v24) {
			var name = _v24.a;
			var value = _v24.b;
			if (!name.$) {
				var argName = name.a;
				return argName + (' = ' + A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, value));
			} else {
				return A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, value);
			}
		};
		if (argValues.b && (!argValues.b.b)) {
			var singleArgValue = argValues.a;
			return $author$project$Morphir$File$SourceCode$parens(
				mapArgValue(singleArgValue));
		} else {
			return $author$project$Morphir$File$SourceCode$concat(
				_List_fromArray(
					[
						'(',
						$author$project$Morphir$File$SourceCode$newLine,
						A2(
						$author$project$Morphir$File$SourceCode$indentLines,
						opt.D,
						A2(
							$elm$core$List$indexedMap,
							F2(
								function (index, argValue) {
									return _Utils_eq(
										index + 1,
										$elm$core$List$length(argValues)) ? mapArgValue(argValue) : $author$project$Morphir$File$SourceCode$concat(
										_List_fromArray(
											[
												mapArgValue(argValue),
												','
											]));
								}),
							argValues)),
						$author$project$Morphir$File$SourceCode$newLine,
						')'
					]));
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapArgDecl = F2(
	function (opt, argDecl) {
		var defaultValueDoc = function () {
			var _v21 = argDecl.ek;
			if (!_v21.$) {
				var value = _v21.a;
				return ' = ' + A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, value);
			} else {
				return $author$project$Morphir$File$SourceCode$empty;
			}
		}();
		return $author$project$Morphir$Scala$PrettyPrinter$mapModifiers(argDecl.e4) + (argDecl.cN + (': ' + (A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, argDecl.gl) + defaultValueDoc)));
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapArgDecls = F2(
	function (opt, argDecls) {
		return '(' + ($author$project$Morphir$File$SourceCode$newLine + (A2(
			$author$project$Morphir$File$SourceCode$indent,
			opt.D,
			$author$project$Morphir$File$SourceCode$concat(
				A2(
					$elm$core$List$intersperse,
					',' + $author$project$Morphir$File$SourceCode$newLine,
					A2(
						$elm$core$List$map,
						$author$project$Morphir$Scala$PrettyPrinter$mapArgDecl(opt),
						argDecls)))) + ($author$project$Morphir$File$SourceCode$newLine + ')')));
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapExtends = F2(
	function (opt, types) {
		if (!types.b) {
			return $author$project$Morphir$File$SourceCode$empty;
		} else {
			return ' extends ' + $author$project$Morphir$File$SourceCode$concat(
				A2(
					$elm$core$List$intersperse,
					' with ',
					A2(
						$elm$core$List$map,
						$author$project$Morphir$Scala$PrettyPrinter$mapType(opt),
						types)));
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapMemberDecl = F2(
	function (opt, memberDecl) {
		switch (memberDecl.$) {
			case 0:
				var typeAlias = memberDecl.a;
				return 'type ' + (typeAlias.dC + (A2($author$project$Morphir$Scala$PrettyPrinter$mapTypeArgs, opt, typeAlias.ac) + (' = ' + A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, typeAlias.gl))));
			case 1:
				var decl = memberDecl.a;
				return $author$project$Morphir$File$SourceCode$concat(
					_List_fromArray(
						[
							'val ',
							$author$project$Morphir$Scala$PrettyPrinter$mapPattern(decl.cU),
							function () {
							var _v16 = decl.$7;
							if (!_v16.$) {
								var tpe = _v16.a;
								return $author$project$Morphir$File$SourceCode$concat(
									_List_fromArray(
										[
											': ',
											A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, tpe)
										]));
							} else {
								return $author$project$Morphir$File$SourceCode$empty;
							}
						}(),
							' = ',
							A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, decl.aT)
						]));
			case 2:
				var decl = memberDecl.a;
				var returnTypeDoc = function () {
					var _v19 = decl.aO;
					if (!_v19.$) {
						var tpe = _v19.a;
						return ': ' + A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, tpe);
					} else {
						return $author$project$Morphir$File$SourceCode$empty;
					}
				}();
				var modifierDoc = $author$project$Morphir$Scala$PrettyPrinter$mapModifiers(decl.e4);
				var bodyDoc = function () {
					var _v18 = decl.dV;
					if (!_v18.$) {
						if (_v18.a.$ === 9) {
							var value = _v18.a;
							return ' = ' + A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, value);
						} else {
							var value = _v18.a;
							return ' =' + ($author$project$Morphir$File$SourceCode$newLine + A2(
								$author$project$Morphir$File$SourceCode$indent,
								opt.D,
								A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, value)));
						}
					} else {
						return $author$project$Morphir$File$SourceCode$empty;
					}
				}();
				var argsDoc = function () {
					var _v17 = decl.dP;
					if (!_v17.b) {
						return $author$project$Morphir$File$SourceCode$empty;
					} else {
						return $author$project$Morphir$File$SourceCode$concat(
							A2(
								$elm$core$List$map,
								$author$project$Morphir$Scala$PrettyPrinter$mapArgDecls(opt),
								decl.dP));
					}
				}();
				return modifierDoc + ('def ' + (decl.cN + (A2($author$project$Morphir$Scala$PrettyPrinter$mapTypeArgs, opt, decl.ac) + (argsDoc + (returnTypeDoc + bodyDoc)))));
			default:
				var decl = memberDecl.a;
				return A2($author$project$Morphir$Scala$PrettyPrinter$mapTypeDecl, opt, decl);
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapType = F2(
	function (opt, tpe) {
		switch (tpe.$) {
			case 0:
				var name = tpe.a;
				return name;
			case 1:
				var path = tpe.a;
				var name = tpe.b;
				return $author$project$Morphir$File$SourceCode$dotSep(
					_Utils_ap(
						path,
						_List_fromArray(
							[name])));
			case 2:
				var ctor = tpe.a;
				var args = tpe.b;
				return A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, ctor) + ('[' + ($author$project$Morphir$File$SourceCode$concat(
					A2(
						$elm$core$List$intersperse,
						', ',
						A2(
							$elm$core$List$map,
							$author$project$Morphir$Scala$PrettyPrinter$mapType(opt),
							args))) + ']'));
			case 4:
				var elemTypes = tpe.a;
				return $author$project$Morphir$File$SourceCode$parens(
					$author$project$Morphir$File$SourceCode$concat(
						A2(
							$elm$core$List$intersperse,
							', ',
							A2(
								$elm$core$List$map,
								$author$project$Morphir$Scala$PrettyPrinter$mapType(opt),
								elemTypes))));
			case 5:
				var memberDecls = tpe.a;
				return $elm$core$List$isEmpty(memberDecls) ? '{}' : $author$project$Morphir$File$SourceCode$concat(
					_List_fromArray(
						[
							'{ ',
							$author$project$Morphir$File$SourceCode$concat(
							A2(
								$elm$core$List$intersperse,
								'; ',
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Scala$PrettyPrinter$mapMemberDecl(opt),
									memberDecls))),
							' }'
						]));
			case 6:
				var argType = tpe.a;
				var returnType = tpe.b;
				return function () {
					if (argType.$ === 6) {
						return $author$project$Morphir$File$SourceCode$parens(
							A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, argType));
					} else {
						return A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, argType);
					}
				}() + (' => ' + A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, returnType));
			case 3:
				var ctor = tpe.a;
				var args = tpe.b;
				var params = tpe.c;
				return A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, ctor) + ($author$project$Morphir$File$SourceCode$newLine + ('[' + ($author$project$Morphir$File$SourceCode$concat(
					A2(
						$elm$core$List$intersperse,
						', ' + $author$project$Morphir$File$SourceCode$newLine,
						A2(
							$elm$core$List$map,
							$author$project$Morphir$Scala$PrettyPrinter$mapType(opt),
							args))) + ('](' + (A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, params) + ')')))));
			default:
				var childType = tpe.a;
				var message = tpe.b;
				return A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, childType) + (' /* ' + (message + ' */ '));
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapTypeArgs = F2(
	function (opt, typeArgs) {
		if (!typeArgs.b) {
			return $author$project$Morphir$File$SourceCode$empty;
		} else {
			var types = typeArgs;
			return '[' + ($author$project$Morphir$File$SourceCode$concat(
				A2(
					$elm$core$List$intersperse,
					', ',
					A2(
						$elm$core$List$map,
						$author$project$Morphir$Scala$PrettyPrinter$mapType(opt),
						types))) + ']');
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapTypeDecl = F2(
	function (opt, typeDecl) {
		switch (typeDecl.$) {
			case 0:
				var decl = typeDecl.a;
				var body = ' {' + ($author$project$Morphir$File$SourceCode$newLine + ($author$project$Morphir$File$SourceCode$newLine + (A2(
					$author$project$Morphir$File$SourceCode$indent,
					opt.D,
					$author$project$Morphir$File$SourceCode$concat(
						A2(
							$elm$core$List$intersperse,
							_Utils_ap($author$project$Morphir$File$SourceCode$newLine, $author$project$Morphir$File$SourceCode$newLine),
							A2(
								$elm$core$List$map,
								$author$project$Morphir$Scala$PrettyPrinter$mapAnnotated(
									$author$project$Morphir$Scala$PrettyPrinter$mapMemberDecl(opt)),
								decl.aK)))) + ($author$project$Morphir$File$SourceCode$newLine + ($author$project$Morphir$File$SourceCode$newLine + '}')))));
				return $author$project$Morphir$Scala$PrettyPrinter$mapModifiers(decl.e4) + ('trait ' + (decl.cN + (A2($author$project$Morphir$Scala$PrettyPrinter$mapTypeArgs, opt, decl.ac) + (A2($author$project$Morphir$Scala$PrettyPrinter$mapExtends, opt, decl.aI) + body))));
			case 1:
				var decl = typeDecl.a;
				var members = function () {
					var _v9 = decl.aK;
					if (!_v9.b) {
						return $author$project$Morphir$File$SourceCode$empty;
					} else {
						return _Utils_ap(
							$author$project$Morphir$File$SourceCode$newLine,
							_Utils_ap(
								$author$project$Morphir$File$SourceCode$newLine,
								_Utils_ap(
									A2(
										$author$project$Morphir$File$SourceCode$indent,
										opt.D,
										$author$project$Morphir$File$SourceCode$concat(
											A2(
												$elm$core$List$intersperse,
												_Utils_ap($author$project$Morphir$File$SourceCode$newLine, $author$project$Morphir$File$SourceCode$newLine),
												A2(
													$elm$core$List$map,
													$author$project$Morphir$Scala$PrettyPrinter$mapAnnotated(
														$author$project$Morphir$Scala$PrettyPrinter$mapMemberDecl(opt)),
													decl.aK)))),
									_Utils_ap($author$project$Morphir$File$SourceCode$newLine, $author$project$Morphir$File$SourceCode$newLine))));
					}
				}();
				var ctorArgsDoc = function () {
					var _v8 = decl.cc;
					if (!_v8.b) {
						return $author$project$Morphir$File$SourceCode$empty;
					} else {
						return $author$project$Morphir$File$SourceCode$concat(
							A2(
								$elm$core$List$map,
								$author$project$Morphir$Scala$PrettyPrinter$mapArgDecls(opt),
								decl.cc));
					}
				}();
				return $author$project$Morphir$Scala$PrettyPrinter$mapModifiers(decl.e4) + ('class ' + (decl.cN + (A2($author$project$Morphir$Scala$PrettyPrinter$mapTypeArgs, opt, decl.ac) + (ctorArgsDoc + (A2($author$project$Morphir$Scala$PrettyPrinter$mapExtends, opt, decl.aI) + ('{' + (members + '}')))))));
			default:
				var decl = typeDecl.a;
				var memberDoc = function () {
					var _v11 = decl.aK;
					if (!_v11.b) {
						return $author$project$Morphir$File$SourceCode$empty;
					} else {
						return _Utils_ap(
							$author$project$Morphir$File$SourceCode$newLine,
							_Utils_ap(
								$author$project$Morphir$File$SourceCode$newLine,
								_Utils_ap(
									A2(
										$author$project$Morphir$File$SourceCode$indent,
										opt.D,
										$author$project$Morphir$File$SourceCode$concat(
											A2(
												$elm$core$List$intersperse,
												_Utils_ap($author$project$Morphir$File$SourceCode$newLine, $author$project$Morphir$File$SourceCode$newLine),
												A2(
													$elm$core$List$map,
													$author$project$Morphir$Scala$PrettyPrinter$mapAnnotated(
														$author$project$Morphir$Scala$PrettyPrinter$mapMemberDecl(opt)),
													decl.aK)))),
									_Utils_ap($author$project$Morphir$File$SourceCode$newLine, $author$project$Morphir$File$SourceCode$newLine))));
					}
				}();
				var bodyDoc = function () {
					var _v10 = decl.dV;
					if (!_v10.$) {
						if (_v10.a.$ === 9) {
							var value = _v10.a;
							return A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, value);
						} else {
							var value = _v10.a;
							return _Utils_ap(
								$author$project$Morphir$File$SourceCode$newLine,
								A2(
									$author$project$Morphir$File$SourceCode$indent,
									opt.D,
									A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, value)));
						}
					} else {
						return $author$project$Morphir$File$SourceCode$empty;
					}
				}();
				return $author$project$Morphir$Scala$PrettyPrinter$mapModifiers(decl.e4) + ('object ' + (decl.cN + (A2($author$project$Morphir$Scala$PrettyPrinter$mapExtends, opt, decl.aI) + ('{' + (memberDoc + (bodyDoc + '}'))))));
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapValue = F2(
	function (opt, value) {
		switch (value.$) {
			case 0:
				var lit = value.a;
				return $author$project$Morphir$Scala$PrettyPrinter$mapLit(lit);
			case 1:
				var name = value.a;
				return name;
			case 2:
				var path = value.a;
				var name = value.b;
				return $author$project$Morphir$File$SourceCode$dotSep(
					_Utils_ap(
						path,
						_List_fromArray(
							[name])));
			case 3:
				var targetValue = value.a;
				var name = value.b;
				return _Utils_ap(
					A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, targetValue),
					_Utils_ap($author$project$Morphir$File$SourceCode$dot, name));
			case 4:
				return '_';
			case 5:
				var funValue = value.a;
				var argValues = value.b;
				return _Utils_ap(
					A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, funValue),
					A2($author$project$Morphir$Scala$PrettyPrinter$argValueBlock, opt, argValues));
			case 6:
				var op = value.a;
				var right = value.b;
				return _Utils_ap(
					op,
					A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, right));
			case 7:
				var left = value.a;
				var op = value.b;
				var right = value.c;
				return $author$project$Morphir$File$SourceCode$parens(
					A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, left)) + (' ' + (op + (' ' + $author$project$Morphir$File$SourceCode$parens(
					A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, right)))));
			case 8:
				var args = value.a;
				var bodyValue = value.b;
				var argDoc = function (_v2) {
					var argName = _v2.a;
					var maybeArgType = _v2.b;
					if (!maybeArgType.$) {
						var argType = maybeArgType.a;
						return $author$project$Morphir$File$SourceCode$concat(
							_List_fromArray(
								[
									argName,
									': ',
									A2($author$project$Morphir$Scala$PrettyPrinter$mapType, opt, argType)
								]));
					} else {
						return argName;
					}
				};
				var argsDoc = $author$project$Morphir$File$SourceCode$parens(
					A2(
						$elm$core$String$join,
						', ',
						A2($elm$core$List$map, argDoc, args)));
				return argsDoc + (' =>' + ($author$project$Morphir$File$SourceCode$newLine + A2(
					$author$project$Morphir$File$SourceCode$indent,
					opt.D,
					A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, bodyValue))));
			case 9:
				var decls = value.a;
				var returnValue = value.b;
				var declDocs = A2(
					$elm$core$List$map,
					$author$project$Morphir$Scala$PrettyPrinter$mapMemberDecl(opt),
					decls);
				var statements = _Utils_ap(
					declDocs,
					_List_fromArray(
						[
							A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, returnValue)
						]));
				return A2(
					$author$project$Morphir$Scala$PrettyPrinter$statementBlock,
					opt,
					A2($elm$core$List$intersperse, $author$project$Morphir$File$SourceCode$empty, statements));
			case 10:
				var cases = value.a;
				return A2(
					$author$project$Morphir$Scala$PrettyPrinter$matchBlock,
					opt,
					A2(
						$elm$core$List$map,
						function (_v3) {
							var pattern = _v3.a;
							var caseValue = _v3.b;
							return _Utils_Tuple2(
								$author$project$Morphir$Scala$PrettyPrinter$mapPattern(pattern),
								A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, caseValue));
						},
						cases));
			case 11:
				var targetValue = value.a;
				var casesValue = value.b;
				return A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, targetValue) + (' match ' + A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, casesValue));
			case 12:
				var condValue = value.a;
				var trueValue = value.b;
				var falseValue = value.c;
				return $author$project$Morphir$File$SourceCode$concat(
					_List_fromArray(
						[
							'if ',
							$author$project$Morphir$File$SourceCode$parens(
							A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, condValue)),
							' ',
							function () {
							if (trueValue.$ === 9) {
								return A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, trueValue);
							} else {
								return A2(
									$author$project$Morphir$Scala$PrettyPrinter$statementBlock,
									opt,
									_List_fromArray(
										[
											A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, trueValue)
										]));
							}
						}(),
							' else ',
							function () {
							switch (falseValue.$) {
								case 12:
									return A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, falseValue);
								case 9:
									return A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, falseValue);
								default:
									return A2(
										$author$project$Morphir$Scala$PrettyPrinter$statementBlock,
										opt,
										_List_fromArray(
											[
												A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, falseValue)
											]));
							}
						}()
						]));
			case 13:
				var elemValues = value.a;
				return $author$project$Morphir$File$SourceCode$parens(
					$author$project$Morphir$File$SourceCode$concat(
						A2(
							$elm$core$List$intersperse,
							', ',
							A2(
								$elm$core$List$map,
								$author$project$Morphir$Scala$PrettyPrinter$mapValue(opt),
								elemValues))));
			case 14:
				var fieldValues = value.a;
				return $elm$core$List$isEmpty(fieldValues) ? 'new {}' : $author$project$Morphir$File$SourceCode$concat(
					_List_fromArray(
						[
							'new {',
							$author$project$Morphir$File$SourceCode$newLine,
							A2(
							$author$project$Morphir$File$SourceCode$indentLines,
							opt.D,
							A2(
								$elm$core$List$map,
								function (_v6) {
									var fieldName = _v6.a;
									var fieldValue = _v6.b;
									return $author$project$Morphir$File$SourceCode$concat(
										_List_fromArray(
											[
												'def ',
												fieldName,
												' = ',
												A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, fieldValue)
											]));
								},
								fieldValues)),
							$author$project$Morphir$File$SourceCode$newLine,
							'}'
						]));
			case 15:
				return '{}';
			case 16:
				return 'this';
			default:
				var childValue = value.a;
				var message = value.b;
				return A2($author$project$Morphir$Scala$PrettyPrinter$mapValue, opt, childValue) + (' /* ' + (message + ' */ '));
		}
	});
var $author$project$Morphir$Scala$PrettyPrinter$mapCompilationUnit = F2(
	function (opt, cu) {
		return $author$project$Morphir$File$SourceCode$concat(
			_List_fromArray(
				[
					$author$project$Morphir$File$SourceCode$concat(
					_List_fromArray(
						[
							'package ',
							$author$project$Morphir$File$SourceCode$dotSep(cu.fs),
							$author$project$Morphir$File$SourceCode$newLine
						])),
					$author$project$Morphir$File$SourceCode$newLine,
					A2(
					$elm$core$String$join,
					_Utils_ap($author$project$Morphir$File$SourceCode$newLine, $author$project$Morphir$File$SourceCode$newLine),
					A2(
						$elm$core$List$map,
						$author$project$Morphir$Scala$PrettyPrinter$mapDocumented(
							$author$project$Morphir$Scala$PrettyPrinter$mapAnnotated(
								$author$project$Morphir$Scala$PrettyPrinter$mapTypeDecl(opt))),
						cu.gm))
				]));
	});
var $author$project$Morphir$Scala$AST$Annotated = F2(
	function (annotations, value) {
		return {dN: annotations, aT: value};
	});
var $author$project$Morphir$Scala$AST$Documented = F2(
	function (doc, value) {
		return {en: doc, aT: value};
	});
var $author$project$Morphir$Scala$AST$FunctionDecl = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Scala$AST$Object = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Scala$AST$Private = function (a) {
	return {$: 6, a: a};
};
var $elm_community$list_extra$List$Extra$last = function (items) {
	last:
	while (true) {
		if (!items.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!items.b.b) {
				var x = items.a;
				return $elm$core$Maybe$Just(x);
			} else {
				var rest = items.b;
				var $temp$items = rest;
				items = $temp$items;
				continue last;
			}
		}
	}
};
var $author$project$Morphir$Scala$AST$Apply = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$ArgValue = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$Block = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$BooleanLit = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Scala$AST$CharacterLit = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Scala$AST$FloatLit = function (a) {
	return {$: 4, a: a};
};
var $author$project$Morphir$Scala$AST$IfElse = F3(
	function (a, b, c) {
		return {$: 12, a: a, b: b, c: c};
	});
var $author$project$Morphir$Scala$AST$IntegerLit = function (a) {
	return {$: 3, a: a};
};
var $author$project$Morphir$Scala$AST$Lambda = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$Literal = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Scala$AST$Match = F2(
	function (a, b) {
		return {$: 11, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$MatchCases = function (a) {
	return {$: 10, a: a};
};
var $author$project$Morphir$Scala$AST$NamedMatch = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Scala$AST$Ref = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$Select = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$StringLit = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Scala$AST$StructuralValue = function (a) {
	return {$: 14, a: a};
};
var $author$project$Morphir$Scala$AST$Tuple = function (a) {
	return {$: 13, a: a};
};
var $author$project$Morphir$Scala$AST$Unit = {$: 15};
var $author$project$Morphir$Scala$AST$ValueDecl = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Scala$AST$Variable = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Scala$AST$Wildcard = {$: 4};
var $author$project$Morphir$IR$Module$definitionToSpecification = function (def) {
	return {
		gn: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$filterMap,
				function (_v0) {
					var path = _v0.a;
					var accessControlledType = _v0.b;
					return A2(
						$elm$core$Maybe$map,
						function (typeDef) {
							return _Utils_Tuple2(
								path,
								A2($author$project$Morphir$IR$Documented$map, $author$project$Morphir$IR$Type$definitionToSpecification, typeDef));
						},
						$author$project$Morphir$IR$AccessControlled$withPublicAccess(accessControlledType));
				},
				$elm$core$Dict$toList(def.gn))),
		gq: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$filterMap,
				function (_v1) {
					var path = _v1.a;
					var accessControlledValue = _v1.b;
					return A2(
						$elm$core$Maybe$map,
						function (valueDef) {
							return _Utils_Tuple2(
								path,
								$author$project$Morphir$IR$Value$definitionToSpecification(valueDef));
						},
						$author$project$Morphir$IR$AccessControlled$withPublicAccess(accessControlledValue));
				},
				$elm$core$Dict$toList(def.gq)))
	};
};
var $author$project$Morphir$IR$Package$definitionToSpecification = function (def) {
	return {
		e7: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$filterMap,
				function (_v0) {
					var path = _v0.a;
					var accessControlledModule = _v0.b;
					return A2(
						$elm$core$Maybe$map,
						function (moduleDef) {
							return _Utils_Tuple2(
								path,
								$author$project$Morphir$IR$Module$definitionToSpecification(moduleDef));
						},
						$author$project$Morphir$IR$AccessControlled$withPublicAccess(accessControlledModule));
				},
				$elm$core$Dict$toList(def.e7)))
	};
};
var $author$project$Morphir$IR$Package$lookupModuleSpecification = F2(
	function (modulePath, packageSpec) {
		return A2($elm$core$Dict$get, modulePath, packageSpec.e7);
	});
var $author$project$Morphir$IR$Distribution$lookupModuleSpecification = F3(
	function (packageName, modulePath, distribution) {
		var libraryPackageName = distribution.a;
		var dependencies = distribution.b;
		var packageDef = distribution.c;
		return _Utils_eq(packageName, libraryPackageName) ? A2(
			$author$project$Morphir$IR$Package$lookupModuleSpecification,
			modulePath,
			$author$project$Morphir$IR$Package$definitionToSpecification(packageDef)) : A2(
			$elm$core$Maybe$andThen,
			$author$project$Morphir$IR$Package$lookupModuleSpecification(modulePath),
			A2($elm$core$Dict$get, packageName, dependencies));
	});
var $author$project$Morphir$IR$Module$lookupTypeSpecification = F2(
	function (localName, moduleSpec) {
		return A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.aT;
			},
			A2($elm$core$Dict$get, localName, moduleSpec.gn));
	});
var $author$project$Morphir$IR$Distribution$lookupBaseTypeName = F2(
	function (fQName, distribution) {
		var packageName = fQName.a;
		var moduleName = fQName.b;
		var localName = fQName.c;
		return A2(
			$elm$core$Maybe$andThen,
			function (typeSpec) {
				if ((!typeSpec.$) && (typeSpec.b.$ === 1)) {
					var _v1 = typeSpec.b;
					var aliasFQName = _v1.b;
					return A2($author$project$Morphir$IR$Distribution$lookupBaseTypeName, aliasFQName, distribution);
				} else {
					return $elm$core$Maybe$Just(fQName);
				}
			},
			A2(
				$elm$core$Maybe$andThen,
				$author$project$Morphir$IR$Module$lookupTypeSpecification(localName),
				A3($author$project$Morphir$IR$Distribution$lookupModuleSpecification, packageName, moduleName, distribution)));
	});
var $author$project$Morphir$Scala$Backend$mapFQNameToPathAndName = function (_v0) {
	var packagePath = _v0.a;
	var modulePath = _v0.b;
	var localName = _v0.c;
	var scalaModulePath = function () {
		var _v1 = $elm$core$List$reverse(modulePath);
		if (!_v1.b) {
			return _List_Nil;
		} else {
			var lastName = _v1.a;
			var reverseModulePath = _v1.b;
			return $elm$core$List$concat(
				_List_fromArray(
					[
						A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toCamelCase, $elm$core$String$toLower),
						packagePath),
						A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toCamelCase, $elm$core$String$toLower),
						$elm$core$List$reverse(reverseModulePath)),
						_List_fromArray(
						[
							$author$project$Morphir$IR$Name$toTitleCase(lastName)
						])
					]));
		}
	}();
	return _Utils_Tuple2(scalaModulePath, localName);
};
var $author$project$Morphir$Scala$AST$AliasedMatch = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$EmptyListMatch = {$: 6};
var $author$project$Morphir$Scala$AST$HeadTailMatch = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$LiteralMatch = function (a) {
	return {$: 3, a: a};
};
var $author$project$Morphir$Scala$AST$TupleMatch = function (a) {
	return {$: 5, a: a};
};
var $author$project$Morphir$Scala$AST$UnapplyMatch = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $author$project$Morphir$Scala$AST$WildcardMatch = {$: 0};
var $author$project$Morphir$Scala$Backend$mapPattern = function (pattern) {
	switch (pattern.$) {
		case 0:
			var a = pattern.a;
			return $author$project$Morphir$Scala$AST$WildcardMatch;
		case 1:
			if (!pattern.b.$) {
				var a = pattern.a;
				var alias = pattern.c;
				return $author$project$Morphir$Scala$AST$NamedMatch(
					$author$project$Morphir$IR$Name$toCamelCase(alias));
			} else {
				var a = pattern.a;
				var aliasedPattern = pattern.b;
				var alias = pattern.c;
				return A2(
					$author$project$Morphir$Scala$AST$AliasedMatch,
					$author$project$Morphir$IR$Name$toCamelCase(alias),
					$author$project$Morphir$Scala$Backend$mapPattern(aliasedPattern));
			}
		case 2:
			var a = pattern.a;
			var itemPatterns = pattern.b;
			return $author$project$Morphir$Scala$AST$TupleMatch(
				A2($elm$core$List$map, $author$project$Morphir$Scala$Backend$mapPattern, itemPatterns));
		case 3:
			var a = pattern.a;
			var fQName = pattern.b;
			var argPatterns = pattern.c;
			var _v1 = $author$project$Morphir$Scala$Backend$mapFQNameToPathAndName(fQName);
			var path = _v1.a;
			var name = _v1.b;
			return A3(
				$author$project$Morphir$Scala$AST$UnapplyMatch,
				path,
				$author$project$Morphir$IR$Name$toTitleCase(name),
				A2($elm$core$List$map, $author$project$Morphir$Scala$Backend$mapPattern, argPatterns));
		case 4:
			var a = pattern.a;
			return $author$project$Morphir$Scala$AST$EmptyListMatch;
		case 5:
			var a = pattern.a;
			var headPattern = pattern.b;
			var tailPattern = pattern.c;
			return A2(
				$author$project$Morphir$Scala$AST$HeadTailMatch,
				$author$project$Morphir$Scala$Backend$mapPattern(headPattern),
				$author$project$Morphir$Scala$Backend$mapPattern(tailPattern));
		case 6:
			var a = pattern.a;
			var literal = pattern.b;
			var map = function (l) {
				switch (l.$) {
					case 0:
						var v = l.a;
						return $author$project$Morphir$Scala$AST$BooleanLit(v);
					case 1:
						var v = l.a;
						return $author$project$Morphir$Scala$AST$CharacterLit(v);
					case 2:
						var v = l.a;
						return $author$project$Morphir$Scala$AST$StringLit(v);
					case 3:
						var v = l.a;
						return $author$project$Morphir$Scala$AST$IntegerLit(v);
					default:
						var v = l.a;
						return $author$project$Morphir$Scala$AST$FloatLit(v);
				}
			};
			return $author$project$Morphir$Scala$AST$LiteralMatch(
				map(literal));
		default:
			var a = pattern.a;
			return $author$project$Morphir$Scala$AST$WildcardMatch;
	}
};
var $author$project$Morphir$Scala$AST$FunctionType = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$StructuralType = function (a) {
	return {$: 5, a: a};
};
var $author$project$Morphir$Scala$AST$TupleType = function (a) {
	return {$: 4, a: a};
};
var $author$project$Morphir$Scala$AST$TypeApply = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$TypeRef = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Morphir$Scala$AST$TypeVar = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Scala$Backend$mapFQNameToTypeRef = function (fQName) {
	var _v0 = $author$project$Morphir$Scala$Backend$mapFQNameToPathAndName(fQName);
	var path = _v0.a;
	var name = _v0.b;
	return A2(
		$author$project$Morphir$Scala$AST$TypeRef,
		path,
		$author$project$Morphir$IR$Name$toTitleCase(name));
};
var $author$project$Morphir$Scala$Backend$mapType = function (tpe) {
	switch (tpe.$) {
		case 0:
			var a = tpe.a;
			var name = tpe.b;
			return $author$project$Morphir$Scala$AST$TypeVar(
				$author$project$Morphir$IR$Name$toTitleCase(name));
		case 1:
			var a = tpe.a;
			var fQName = tpe.b;
			var argTypes = tpe.c;
			var typeRef = $author$project$Morphir$Scala$Backend$mapFQNameToTypeRef(fQName);
			return $elm$core$List$isEmpty(argTypes) ? typeRef : A2(
				$author$project$Morphir$Scala$AST$TypeApply,
				typeRef,
				A2($elm$core$List$map, $author$project$Morphir$Scala$Backend$mapType, argTypes));
		case 2:
			var a = tpe.a;
			var elemTypes = tpe.b;
			return $author$project$Morphir$Scala$AST$TupleType(
				A2($elm$core$List$map, $author$project$Morphir$Scala$Backend$mapType, elemTypes));
		case 3:
			var a = tpe.a;
			var fields = tpe.b;
			return $author$project$Morphir$Scala$AST$StructuralType(
				A2(
					$elm$core$List$map,
					function (field) {
						return $author$project$Morphir$Scala$AST$FunctionDecl(
							{
								dP: _List_Nil,
								dV: $elm$core$Maybe$Nothing,
								e4: _List_Nil,
								cN: $author$project$Morphir$IR$Name$toCamelCase(field.cN),
								aO: $elm$core$Maybe$Just(
									$author$project$Morphir$Scala$Backend$mapType(field.gl)),
								ac: _List_Nil
							});
					},
					fields));
		case 4:
			var a = tpe.a;
			var argName = tpe.b;
			var fields = tpe.c;
			return $author$project$Morphir$Scala$AST$StructuralType(
				A2(
					$elm$core$List$map,
					function (field) {
						return $author$project$Morphir$Scala$AST$FunctionDecl(
							{
								dP: _List_Nil,
								dV: $elm$core$Maybe$Nothing,
								e4: _List_Nil,
								cN: $author$project$Morphir$IR$Name$toCamelCase(field.cN),
								aO: $elm$core$Maybe$Just(
									$author$project$Morphir$Scala$Backend$mapType(field.gl)),
								ac: _List_Nil
							});
					},
					fields));
		case 5:
			var a = tpe.a;
			var argType = tpe.b;
			var returnType = tpe.c;
			return A2(
				$author$project$Morphir$Scala$AST$FunctionType,
				$author$project$Morphir$Scala$Backend$mapType(argType),
				$author$project$Morphir$Scala$Backend$mapType(returnType));
		default:
			var a = tpe.a;
			return A2(
				$author$project$Morphir$Scala$AST$TypeRef,
				_List_fromArray(
					['scala']),
				'Unit');
	}
};
var $author$project$Morphir$IR$Value$uncurryApply = F2(
	function (fun, lastArg) {
		if (fun.$ === 9) {
			var nestedFun = fun.b;
			var nestedArg = fun.c;
			var _v1 = A2($author$project$Morphir$IR$Value$uncurryApply, nestedFun, nestedArg);
			var f = _v1.a;
			var initArgs = _v1.b;
			return _Utils_Tuple2(
				f,
				A2(
					$elm$core$List$append,
					initArgs,
					_List_fromArray(
						[lastArg])));
		} else {
			return _Utils_Tuple2(
				fun,
				_List_fromArray(
					[lastArg]));
		}
	});
var $author$project$Morphir$Scala$Backend$mapFunctionBody = F2(
	function (distribution, val) {
		var mapValue = function (value) {
			switch (value.$) {
				case 0:
					var tpe = value.a;
					var literal = value.b;
					var wrap = F3(
						function (modulePath, moduleName, lit) {
							return A2(
								$author$project$Morphir$Scala$AST$Apply,
								A2($author$project$Morphir$Scala$AST$Ref, modulePath, moduleName),
								_List_fromArray(
									[
										A2(
										$author$project$Morphir$Scala$AST$ArgValue,
										$elm$core$Maybe$Nothing,
										$author$project$Morphir$Scala$AST$Literal(lit))
									]));
						});
					switch (literal.$) {
						case 0:
							var v = literal.a;
							return $author$project$Morphir$Scala$AST$Literal(
								$author$project$Morphir$Scala$AST$BooleanLit(v));
						case 1:
							var v = literal.a;
							return A3(
								wrap,
								_List_fromArray(
									['morphir', 'sdk', 'Char']),
								'from',
								$author$project$Morphir$Scala$AST$CharacterLit(v));
						case 2:
							var v = literal.a;
							return $author$project$Morphir$Scala$AST$Literal(
								$author$project$Morphir$Scala$AST$StringLit(v));
						case 3:
							var v = literal.a;
							if ((tpe.$ === 1) && (!tpe.c.b)) {
								var fQName = tpe.b;
								return _Utils_eq(
									A2($author$project$Morphir$IR$Distribution$lookupBaseTypeName, fQName, distribution),
									$elm$core$Maybe$Just(
										A3($author$project$Morphir$IR$FQName$fqn, 'Morphir.SDK', 'Basics', 'Float'))) ? A3(
									wrap,
									_List_fromArray(
										['morphir', 'sdk', 'Basics']),
									'Float',
									$author$project$Morphir$Scala$AST$IntegerLit(v)) : A3(
									wrap,
									_List_fromArray(
										['morphir', 'sdk', 'Basics']),
									'Int',
									$author$project$Morphir$Scala$AST$IntegerLit(v));
							} else {
								return A3(
									wrap,
									_List_fromArray(
										['morphir', 'sdk', 'Basics']),
									'Int',
									$author$project$Morphir$Scala$AST$IntegerLit(v));
							}
						default:
							var v = literal.a;
							return A3(
								wrap,
								_List_fromArray(
									['morphir', 'sdk', 'Basics']),
								'Float',
								$author$project$Morphir$Scala$AST$FloatLit(v));
					}
				case 1:
					var a = value.a;
					var fQName = value.b;
					var _v3 = $author$project$Morphir$Scala$Backend$mapFQNameToPathAndName(fQName);
					var path = _v3.a;
					var name = _v3.b;
					return A2(
						$author$project$Morphir$Scala$AST$Ref,
						path,
						$author$project$Morphir$IR$Name$toTitleCase(name));
				case 2:
					var a = value.a;
					var elemValues = value.b;
					return $author$project$Morphir$Scala$AST$Tuple(
						A2($elm$core$List$map, mapValue, elemValues));
				case 3:
					var a = value.a;
					var itemValues = value.b;
					return A2(
						$author$project$Morphir$Scala$AST$Apply,
						A2(
							$author$project$Morphir$Scala$AST$Ref,
							_List_fromArray(
								['morphir', 'sdk']),
							'List'),
						A2(
							$elm$core$List$map,
							$author$project$Morphir$Scala$AST$ArgValue($elm$core$Maybe$Nothing),
							A2($elm$core$List$map, mapValue, itemValues)));
				case 4:
					var a = value.a;
					var fieldValues = value.b;
					return $author$project$Morphir$Scala$AST$StructuralValue(
						A2(
							$elm$core$List$map,
							function (_v4) {
								var fieldName = _v4.a;
								var fieldValue = _v4.b;
								return _Utils_Tuple2(
									$author$project$Morphir$IR$Name$toCamelCase(fieldName),
									mapValue(fieldValue));
							},
							fieldValues));
				case 5:
					var a = value.a;
					var name = value.b;
					return $author$project$Morphir$Scala$AST$Variable(
						$author$project$Morphir$IR$Name$toCamelCase(name));
				case 6:
					var a = value.a;
					var fQName = value.b;
					var _v5 = $author$project$Morphir$Scala$Backend$mapFQNameToPathAndName(fQName);
					var path = _v5.a;
					var name = _v5.b;
					return A2(
						$author$project$Morphir$Scala$AST$Ref,
						path,
						$author$project$Morphir$IR$Name$toCamelCase(name));
				case 7:
					var a = value.a;
					var subjectValue = value.b;
					var fieldName = value.c;
					return A2(
						$author$project$Morphir$Scala$AST$Select,
						mapValue(subjectValue),
						$author$project$Morphir$IR$Name$toCamelCase(fieldName));
				case 8:
					var tpe = value.a;
					var fieldName = value.b;
					if (tpe.$ === 5) {
						var inputType = tpe.b;
						return A2(
							$author$project$Morphir$Scala$AST$Lambda,
							_List_fromArray(
								[
									_Utils_Tuple2(
									'x',
									$elm$core$Maybe$Just(
										$author$project$Morphir$Scala$Backend$mapType(inputType)))
								]),
							A2(
								$author$project$Morphir$Scala$AST$Select,
								$author$project$Morphir$Scala$AST$Variable('x'),
								$author$project$Morphir$IR$Name$toCamelCase(fieldName)));
					} else {
						return A2(
							$author$project$Morphir$Scala$AST$Select,
							$author$project$Morphir$Scala$AST$Wildcard,
							$author$project$Morphir$IR$Name$toCamelCase(fieldName));
					}
				case 9:
					var a = value.a;
					var fun = value.b;
					var arg = value.c;
					var _v7 = A2($author$project$Morphir$IR$Value$uncurryApply, fun, arg);
					var bottomFun = _v7.a;
					var args = _v7.b;
					if (bottomFun.$ === 6) {
						return A2(
							$author$project$Morphir$Scala$AST$Apply,
							mapValue(fun),
							_List_fromArray(
								[
									A2(
									$author$project$Morphir$Scala$AST$ArgValue,
									$elm$core$Maybe$Nothing,
									mapValue(arg))
								]));
					} else {
						return A2(
							$author$project$Morphir$Scala$AST$Apply,
							mapValue(bottomFun),
							A2(
								$elm$core$List$map,
								function (argValue) {
									return A2(
										$author$project$Morphir$Scala$AST$ArgValue,
										$elm$core$Maybe$Nothing,
										mapValue(argValue));
								},
								args));
					}
				case 10:
					var a = value.a;
					var argPattern = value.b;
					var bodyValue = value.c;
					if ((argPattern.$ === 1) && (!argPattern.b.$)) {
						var tpe = argPattern.a;
						var alias = argPattern.c;
						return A2(
							$author$project$Morphir$Scala$AST$Lambda,
							_List_fromArray(
								[
									_Utils_Tuple2(
									$author$project$Morphir$IR$Name$toCamelCase(alias),
									$elm$core$Maybe$Just(
										$author$project$Morphir$Scala$Backend$mapType(tpe)))
								]),
							mapValue(bodyValue));
					} else {
						return $author$project$Morphir$Scala$AST$MatchCases(
							_List_fromArray(
								[
									_Utils_Tuple2(
									$author$project$Morphir$Scala$Backend$mapPattern(argPattern),
									mapValue(bodyValue))
								]));
					}
				case 11:
					var flattenLetDef = function (v) {
						if (v.$ === 11) {
							var a = v.a;
							var dName = v.b;
							var d = v.c;
							var inV = v.d;
							var _v11 = flattenLetDef(inV);
							var nestedDefs = _v11.a;
							var nestedInValue = _v11.b;
							return _Utils_Tuple2(
								A2(
									$elm$core$List$cons,
									_Utils_Tuple2(dName, d),
									nestedDefs),
								nestedInValue);
						} else {
							return _Utils_Tuple2(_List_Nil, v);
						}
					};
					var _v12 = flattenLetDef(value);
					var defs = _v12.a;
					var finalInValue = _v12.b;
					return A2(
						$author$project$Morphir$Scala$AST$Block,
						A2(
							$elm$core$List$map,
							function (_v13) {
								var defName = _v13.a;
								var def = _v13.b;
								return $elm$core$List$isEmpty(def.a3) ? $author$project$Morphir$Scala$AST$ValueDecl(
									{
										e4: _List_Nil,
										cU: $author$project$Morphir$Scala$AST$NamedMatch(
											$author$project$Morphir$IR$Name$toCamelCase(defName)),
										aT: mapValue(def.dV),
										$7: $elm$core$Maybe$Just(
											$author$project$Morphir$Scala$Backend$mapType(def.fq))
									}) : $author$project$Morphir$Scala$AST$FunctionDecl(
									{
										dP: _List_fromArray(
											[
												A2(
												$elm$core$List$map,
												function (_v14) {
													var argName = _v14.a;
													var argType = _v14.c;
													return {
														ek: $elm$core$Maybe$Nothing,
														e4: _List_Nil,
														cN: $author$project$Morphir$IR$Name$toCamelCase(argName),
														gl: $author$project$Morphir$Scala$Backend$mapType(argType)
													};
												},
												def.a3)
											]),
										dV: $elm$core$Maybe$Just(
											mapValue(def.dV)),
										e4: _List_Nil,
										cN: $author$project$Morphir$IR$Name$toCamelCase(defName),
										aO: $elm$core$Maybe$Just(
											$author$project$Morphir$Scala$Backend$mapType(def.fq)),
										ac: _List_Nil
									});
							},
							defs),
						mapValue(finalInValue));
				case 12:
					var a = value.a;
					var defs = value.b;
					var inValue = value.c;
					return A2(
						$author$project$Morphir$Scala$AST$Block,
						A2(
							$elm$core$List$map,
							function (_v15) {
								var defName = _v15.a;
								var def = _v15.b;
								return $author$project$Morphir$Scala$AST$FunctionDecl(
									{
										dP: $elm$core$List$isEmpty(def.a3) ? _List_Nil : _List_fromArray(
											[
												A2(
												$elm$core$List$map,
												function (_v16) {
													var argName = _v16.a;
													var argType = _v16.c;
													return {
														ek: $elm$core$Maybe$Nothing,
														e4: _List_Nil,
														cN: $author$project$Morphir$IR$Name$toCamelCase(argName),
														gl: $author$project$Morphir$Scala$Backend$mapType(argType)
													};
												},
												def.a3)
											]),
										dV: $elm$core$Maybe$Just(
											mapValue(def.dV)),
										e4: _List_Nil,
										cN: $author$project$Morphir$IR$Name$toCamelCase(defName),
										aO: $elm$core$Maybe$Just(
											$author$project$Morphir$Scala$Backend$mapType(def.fq)),
										ac: _List_Nil
									});
							},
							$elm$core$Dict$toList(defs)),
						mapValue(inValue));
				case 13:
					var a = value.a;
					var bindPattern = value.b;
					var bindValue = value.c;
					var inValue = value.d;
					return A2(
						$author$project$Morphir$Scala$AST$Block,
						_List_fromArray(
							[
								$author$project$Morphir$Scala$AST$ValueDecl(
								{
									e4: _List_Nil,
									cU: $author$project$Morphir$Scala$Backend$mapPattern(bindPattern),
									aT: mapValue(bindValue),
									$7: $elm$core$Maybe$Nothing
								})
							]),
						mapValue(inValue));
				case 14:
					var a = value.a;
					var condValue = value.b;
					var thenValue = value.c;
					var elseValue = value.d;
					return A3(
						$author$project$Morphir$Scala$AST$IfElse,
						mapValue(condValue),
						mapValue(thenValue),
						mapValue(elseValue));
				case 15:
					var a = value.a;
					var onValue = value.b;
					var cases = value.c;
					return A2(
						$author$project$Morphir$Scala$AST$Match,
						mapValue(onValue),
						$author$project$Morphir$Scala$AST$MatchCases(
							A2(
								$elm$core$List$map,
								function (_v17) {
									var casePattern = _v17.a;
									var caseValue = _v17.b;
									return _Utils_Tuple2(
										$author$project$Morphir$Scala$Backend$mapPattern(casePattern),
										mapValue(caseValue));
								},
								cases)));
				case 16:
					var a = value.a;
					var subjectValue = value.b;
					var fieldUpdates = value.c;
					return A2(
						$author$project$Morphir$Scala$AST$Apply,
						A2(
							$author$project$Morphir$Scala$AST$Select,
							mapValue(subjectValue),
							'copy'),
						A2(
							$elm$core$List$map,
							function (_v18) {
								var fieldName = _v18.a;
								var fieldValue = _v18.b;
								return A2(
									$author$project$Morphir$Scala$AST$ArgValue,
									$elm$core$Maybe$Just(
										$author$project$Morphir$IR$Name$toCamelCase(fieldName)),
									mapValue(fieldValue));
							},
							fieldUpdates));
				default:
					var a = value.a;
					return $author$project$Morphir$Scala$AST$Unit;
			}
		};
		return mapValue(val);
	});
var $author$project$Morphir$Scala$AST$Case = {$: 2};
var $author$project$Morphir$Scala$AST$Class = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Scala$AST$MemberTypeDecl = function (a) {
	return {$: 3, a: a};
};
var $author$project$Morphir$Scala$AST$TypeAlias = function (a) {
	return {$: 0, a: a};
};
var $author$project$Morphir$Scala$AST$Sealed = {$: 0};
var $author$project$Morphir$Scala$AST$Trait = function (a) {
	return {$: 0, a: a};
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$Morphir$Scala$AST$withoutAnnotation = function (a) {
	return A2($author$project$Morphir$Scala$AST$Annotated, _List_Nil, a);
};
var $author$project$Morphir$Scala$Backend$mapCustomTypeDefinition = F6(
	function (currentPackagePath, currentModulePath, moduleDef, typeName, typeParams, accessControlledCtors) {
		var parentTraitRef = $author$project$Morphir$Scala$Backend$mapFQNameToTypeRef(
			A3($author$project$Morphir$IR$FQName$FQName, currentPackagePath, currentModulePath, typeName));
		var caseClass = F3(
			function (name, args, _extends) {
				return $elm$core$List$isEmpty(args) ? $author$project$Morphir$Scala$AST$Object(
					{
						dV: $elm$core$Maybe$Nothing,
						aI: _extends,
						aK: _List_Nil,
						e4: _List_fromArray(
							[$author$project$Morphir$Scala$AST$Case]),
						cN: $author$project$Morphir$IR$Name$toTitleCase(name)
					}) : $author$project$Morphir$Scala$AST$Class(
					{
						cc: $elm$core$List$singleton(
							A2(
								$elm$core$List$map,
								function (_v3) {
									var argName = _v3.a;
									var argType = _v3.b;
									return {
										ek: $elm$core$Maybe$Nothing,
										e4: _List_Nil,
										cN: $author$project$Morphir$IR$Name$toCamelCase(argName),
										gl: $author$project$Morphir$Scala$Backend$mapType(argType)
									};
								},
								args)),
						aI: _extends,
						aK: _List_Nil,
						e4: _List_fromArray(
							[$author$project$Morphir$Scala$AST$Case]),
						cN: $author$project$Morphir$IR$Name$toTitleCase(name),
						ac: A2(
							$elm$core$List$map,
							A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toTitleCase, $author$project$Morphir$Scala$AST$TypeVar),
							typeParams)
					});
			});
		var sealedTraitHierarchy = $elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[
						$author$project$Morphir$Scala$AST$Trait(
						{
							aI: _List_Nil,
							aK: _List_Nil,
							e4: _List_fromArray(
								[$author$project$Morphir$Scala$AST$Sealed]),
							cN: $author$project$Morphir$IR$Name$toTitleCase(typeName),
							ac: A2(
								$elm$core$List$map,
								A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toTitleCase, $author$project$Morphir$Scala$AST$TypeVar),
								typeParams)
						})
					]),
					A2(
					$elm$core$List$map,
					function (_v2) {
						var ctorName = _v2.a;
						var ctorArgs = _v2.b;
						return A3(
							caseClass,
							ctorName,
							ctorArgs,
							$elm$core$List$isEmpty(typeParams) ? _List_fromArray(
								[parentTraitRef]) : _List_fromArray(
								[
									A2(
									$author$project$Morphir$Scala$AST$TypeApply,
									parentTraitRef,
									A2(
										$elm$core$List$map,
										A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toTitleCase, $author$project$Morphir$Scala$AST$TypeVar),
										typeParams))
								]));
					},
					accessControlledCtors.aT)
				]));
		var _v0 = accessControlledCtors.aT;
		if (_v0.b && (!_v0.b.b)) {
			var _v1 = _v0.a;
			var ctorName = _v1.a;
			var ctorArgs = _v1.b;
			return _Utils_eq(ctorName, typeName) ? _List_fromArray(
				[
					$author$project$Morphir$Scala$AST$withoutAnnotation(
					$author$project$Morphir$Scala$AST$MemberTypeDecl(
						A3(caseClass, ctorName, ctorArgs, _List_Nil)))
				]) : A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeR, $author$project$Morphir$Scala$AST$MemberTypeDecl, $author$project$Morphir$Scala$AST$withoutAnnotation),
				sealedTraitHierarchy);
		} else {
			return A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeR, $author$project$Morphir$Scala$AST$MemberTypeDecl, $author$project$Morphir$Scala$AST$withoutAnnotation),
				sealedTraitHierarchy);
		}
	});
var $author$project$Morphir$Scala$Backend$mapTypeMember = F4(
	function (currentPackagePath, currentModulePath, accessControlledModuleDef, _v0) {
		var typeName = _v0.a;
		var accessControlledDocumentedTypeDef = _v0.b;
		var _v1 = accessControlledDocumentedTypeDef.aT.aT;
		if (!_v1.$) {
			if (_v1.b.$ === 3) {
				var typeParams = _v1.a;
				var _v2 = _v1.b;
				var fields = _v2.b;
				return _List_fromArray(
					[
						$author$project$Morphir$Scala$AST$withoutAnnotation(
						$author$project$Morphir$Scala$AST$MemberTypeDecl(
							$author$project$Morphir$Scala$AST$Class(
								{
									cc: $elm$core$List$singleton(
										A2(
											$elm$core$List$map,
											function (field) {
												return {
													ek: $elm$core$Maybe$Nothing,
													e4: _List_Nil,
													cN: $author$project$Morphir$IR$Name$toCamelCase(field.cN),
													gl: $author$project$Morphir$Scala$Backend$mapType(field.gl)
												};
											},
											fields)),
									aI: _List_Nil,
									aK: _List_Nil,
									e4: _List_fromArray(
										[$author$project$Morphir$Scala$AST$Case]),
									cN: $author$project$Morphir$IR$Name$toTitleCase(typeName),
									ac: A2(
										$elm$core$List$map,
										A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toTitleCase, $author$project$Morphir$Scala$AST$TypeVar),
										typeParams)
								})))
					]);
			} else {
				var typeParams = _v1.a;
				var typeExp = _v1.b;
				return _List_fromArray(
					[
						$author$project$Morphir$Scala$AST$withoutAnnotation(
						$author$project$Morphir$Scala$AST$TypeAlias(
							{
								dC: $author$project$Morphir$IR$Name$toTitleCase(typeName),
								gl: $author$project$Morphir$Scala$Backend$mapType(typeExp),
								ac: A2(
									$elm$core$List$map,
									A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toTitleCase, $author$project$Morphir$Scala$AST$TypeVar),
									typeParams)
							}))
					]);
			}
		} else {
			var typeParams = _v1.a;
			var accessControlledCtors = _v1.b;
			return A6($author$project$Morphir$Scala$Backend$mapCustomTypeDefinition, currentPackagePath, currentModulePath, accessControlledModuleDef.aT, typeName, typeParams, accessControlledCtors);
		}
	});
var $author$project$Morphir$Scala$Backend$mapModuleDefinition = F5(
	function (opt, distribution, currentPackagePath, currentModulePath, accessControlledModuleDef) {
		var typeMembers = A2(
			$elm$core$List$concatMap,
			function (types) {
				return A4($author$project$Morphir$Scala$Backend$mapTypeMember, currentPackagePath, currentModulePath, accessControlledModuleDef, types);
			},
			$elm$core$Dict$toList(accessControlledModuleDef.aT.gn));
		var functionMembers = A2(
			$elm$core$List$map,
			$author$project$Morphir$Scala$AST$withoutAnnotation,
			A2(
				$elm$core$List$concatMap,
				function (_v3) {
					var valueName = _v3.a;
					var accessControlledValueDef = _v3.b;
					return _List_fromArray(
						[
							$author$project$Morphir$Scala$AST$FunctionDecl(
							{
								dP: $elm$core$List$isEmpty(accessControlledValueDef.aT.a3) ? _List_Nil : A2(
									$elm$core$List$map,
									function (_v4) {
										var argName = _v4.a;
										var a = _v4.b;
										var argType = _v4.c;
										return _List_fromArray(
											[
												{
												ek: $elm$core$Maybe$Nothing,
												e4: _List_Nil,
												cN: $author$project$Morphir$IR$Name$toCamelCase(argName),
												gl: $author$project$Morphir$Scala$Backend$mapType(argType)
											}
											]);
									},
									accessControlledValueDef.aT.a3),
								dV: $elm$core$Maybe$Just(
									A2($author$project$Morphir$Scala$Backend$mapFunctionBody, distribution, accessControlledValueDef.aT.dV)),
								e4: function () {
									var _v5 = accessControlledValueDef.b0;
									if (!_v5) {
										return _List_Nil;
									} else {
										return _List_fromArray(
											[
												$author$project$Morphir$Scala$AST$Private($elm$core$Maybe$Nothing)
											]);
									}
								}(),
								cN: $author$project$Morphir$IR$Name$toCamelCase(valueName),
								aO: $elm$core$Maybe$Just(
									$author$project$Morphir$Scala$Backend$mapType(accessControlledValueDef.aT.fq)),
								ac: _List_Nil
							})
						]);
				},
				$elm$core$Dict$toList(accessControlledModuleDef.aT.gq)));
		var _v0 = function () {
			var _v1 = $elm$core$List$reverse(currentModulePath);
			if (!_v1.b) {
				return _Utils_Tuple2(_List_Nil, _List_Nil);
			} else {
				var lastName = _v1.a;
				var reverseModulePath = _v1.b;
				return _Utils_Tuple2(
					A2(
						$elm$core$List$append,
						A2(
							$elm$core$List$map,
							A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toCamelCase, $elm$core$String$toLower),
							currentPackagePath),
						A2(
							$elm$core$List$map,
							A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toCamelCase, $elm$core$String$toLower),
							$elm$core$List$reverse(reverseModulePath))),
					lastName);
			}
		}();
		var scalaPackagePath = _v0.a;
		var moduleName = _v0.b;
		var moduleUnit = {
			em: scalaPackagePath,
			ez: $author$project$Morphir$IR$Name$toTitleCase(moduleName) + '.scala',
			eM: _List_Nil,
			fs: scalaPackagePath,
			gm: _List_fromArray(
				[
					A2(
					$author$project$Morphir$Scala$AST$Documented,
					$elm$core$Maybe$Just(
						A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									'Generated based on ',
									A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', currentModulePath)
								]))),
					A2(
						$author$project$Morphir$Scala$AST$Annotated,
						_List_Nil,
						$author$project$Morphir$Scala$AST$Object(
							{
								dV: $elm$core$Maybe$Nothing,
								aI: _List_Nil,
								aK: A2($elm$core$List$append, typeMembers, functionMembers),
								e4: function () {
									var _v2 = accessControlledModuleDef.b0;
									if (!_v2) {
										return _List_Nil;
									} else {
										return _List_fromArray(
											[
												$author$project$Morphir$Scala$AST$Private(
												A2(
													$elm$core$Maybe$map,
													A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toCamelCase, $elm$core$String$toLower),
													$elm_community$list_extra$List$Extra$last(currentPackagePath)))
											]);
									}
								}(),
								cN: $author$project$Morphir$IR$Name$toTitleCase(moduleName)
							})))
				])
		};
		return _List_fromArray(
			[moduleUnit]);
	});
var $author$project$Morphir$Scala$Backend$mapPackageDefinition = F4(
	function (opt, distribution, packagePath, packageDef) {
		return $elm$core$Dict$fromList(
			A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var modulePath = _v0.a;
					var moduleImpl = _v0.b;
					return A2(
						$elm$core$List$map,
						function (compilationUnit) {
							var fileContent = A2(
								$author$project$Morphir$Scala$PrettyPrinter$mapCompilationUnit,
								A2($author$project$Morphir$Scala$PrettyPrinter$Options, 2, 80),
								compilationUnit);
							return _Utils_Tuple2(
								_Utils_Tuple2(compilationUnit.em, compilationUnit.ez),
								fileContent);
						},
						A5($author$project$Morphir$Scala$Backend$mapModuleDefinition, opt, distribution, packagePath, modulePath, moduleImpl));
				},
				$elm$core$Dict$toList(packageDef.e7)));
	});
var $author$project$Morphir$Scala$Backend$mapDistribution = F2(
	function (opt, distro) {
		var packagePath = distro.a;
		var dependencies = distro.b;
		var packageDef = distro.c;
		return A4($author$project$Morphir$Scala$Backend$mapPackageDefinition, opt, distro, packagePath, packageDef);
	});
var $author$project$Morphir$Scala$AST$BinOp = F3(
	function (a, b, c) {
		return {$: 7, a: a, b: b, c: c};
	});
var $author$project$Morphir$SpringBoot$Backend$getScalaPackagePath = F2(
	function (currentPackagePath, currentModulePath) {
		var _v0 = $elm$core$List$reverse(currentModulePath);
		if (!_v0.b) {
			return _Utils_Tuple2(_List_Nil, _List_Nil);
		} else {
			var lastName = _v0.a;
			var reverseModulePath = _v0.b;
			return _Utils_Tuple2(
				A2(
					$elm$core$List$append,
					A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toCamelCase, $elm$core$String$toLower),
						currentPackagePath),
					A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeR, $author$project$Morphir$IR$Name$toCamelCase, $elm$core$String$toLower),
						$elm$core$List$reverse(reverseModulePath))),
				lastName);
		}
	});
var $author$project$Morphir$SpringBoot$Backend$mapMainApp = F2(
	function (currentPackagePath, currentModulePath) {
		var scalaPackagePath = A2($author$project$Morphir$SpringBoot$Backend$getScalaPackagePath, currentPackagePath, currentModulePath).a;
		var moduleMainApp = {
			em: scalaPackagePath,
			ez: 'MainApplication' + '.scala',
			eM: _List_Nil,
			fs: scalaPackagePath,
			gm: _List_fromArray(
				[
					A2(
					$author$project$Morphir$Scala$AST$Documented,
					$elm$core$Maybe$Just(
						A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									'Generated based on ',
									A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', currentModulePath)
								]))),
					A2(
						$author$project$Morphir$Scala$AST$Annotated,
						_List_fromArray(
							['@org.springframework.boot.autoconfigure.SpringBootApplication']),
						$author$project$Morphir$Scala$AST$Class(
							{
								cc: _List_Nil,
								aI: _List_Nil,
								aK: _List_fromArray(
									[
										A2(
										$author$project$Morphir$Scala$AST$Annotated,
										_List_fromArray(
											['@org.springframework.beans.factory.annotation.Autowired']),
										$author$project$Morphir$Scala$AST$ValueDecl(
											{
												e4: _List_fromArray(
													[
														$author$project$Morphir$Scala$AST$Private($elm$core$Maybe$Nothing)
													]),
												cU: $author$project$Morphir$Scala$AST$NamedMatch('servletContext'),
												aT: $author$project$Morphir$Scala$AST$Variable('null'),
												$7: $elm$core$Maybe$Just(
													$author$project$Morphir$Scala$AST$TypeVar('javax.servlet.ServletContext'))
											})),
										A2(
										$author$project$Morphir$Scala$AST$Annotated,
										_List_fromArray(
											['@org.springframework.context.annotation.Bean']),
										$author$project$Morphir$Scala$AST$FunctionDecl(
											{
												dP: _List_Nil,
												dV: $elm$core$Maybe$Just(
													A2(
														$author$project$Morphir$Scala$AST$Block,
														_List_Nil,
														A3(
															$author$project$Morphir$Scala$AST$BinOp,
															A2(
																$author$project$Morphir$Scala$AST$Apply,
																A2(
																	$author$project$Morphir$Scala$AST$Ref,
																	_List_fromArray(
																		['servletContext']),
																	'setAttribute'),
																_List_fromArray(
																	[
																		A2(
																		$author$project$Morphir$Scala$AST$ArgValue,
																		$elm$core$Maybe$Nothing,
																		$author$project$Morphir$Scala$AST$Variable('com.codahale.metrics.servlets.MetricsServlet.METRICS_REGISTRY')),
																		A2(
																		$author$project$Morphir$Scala$AST$ArgValue,
																		$elm$core$Maybe$Nothing,
																		$author$project$Morphir$Scala$AST$Variable('morphir.reference.model.MainApplication.metricRegistry'))
																	])),
															$author$project$Morphir$File$SourceCode$newLine,
															A2(
																$author$project$Morphir$Scala$AST$Apply,
																A2($author$project$Morphir$Scala$AST$Ref, _List_Nil, 'new org.springframework.boot.web.servlet.ServletRegistrationBean'),
																_List_fromArray(
																	[
																		A2(
																		$author$project$Morphir$Scala$AST$ArgValue,
																		$elm$core$Maybe$Nothing,
																		$author$project$Morphir$Scala$AST$Variable('new com.codahale.metrics.servlets.MetricsServlet()')),
																		A2(
																		$author$project$Morphir$Scala$AST$ArgValue,
																		$elm$core$Maybe$Nothing,
																		$author$project$Morphir$Scala$AST$Variable('\"/metrics\"'))
																	]))))),
												e4: _List_Nil,
												cN: 'adminServletRegistrationBean',
												aO: $elm$core$Maybe$Nothing,
												ac: _List_Nil
											})),
										A2(
										$author$project$Morphir$Scala$AST$Annotated,
										_List_fromArray(
											['@org.springframework.context.annotation.Bean']),
										$author$project$Morphir$Scala$AST$FunctionDecl(
											{
												dP: _List_Nil,
												dV: $elm$core$Maybe$Just(
													A2(
														$author$project$Morphir$Scala$AST$Select,
														A2(
															$author$project$Morphir$Scala$AST$Ref,
															_List_fromArray(
																[
																	'new springfox.documentation.spring.web.plugins.Docket(springfox.documentation.spi.DocumentationType.SWAGGER_2)',
																	'select',
																	'apis(springfox.documentation.builders.RequestHandlerSelectors.basePackage( \"' + ($author$project$Morphir$File$SourceCode$dotSep(scalaPackagePath) + '\" ))')
																]),
															'paths(springfox.documentation.builders.PathSelectors.any)'),
														'build')),
												e4: _List_Nil,
												cN: 'api',
												aO: $elm$core$Maybe$Just(
													$author$project$Morphir$Scala$AST$TypeVar('springfox.documentation.spring.web.plugins.Docket')),
												ac: _List_Nil
											}))
									]),
								e4: _List_Nil,
								cN: 'MainApplication',
								ac: _List_Nil
							}))),
					A2(
					$author$project$Morphir$Scala$AST$Documented,
					$elm$core$Maybe$Just(
						A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									'Generated based on ',
									A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', currentModulePath)
								]))),
					$author$project$Morphir$Scala$AST$withoutAnnotation(
						$author$project$Morphir$Scala$AST$Object(
							{
								dV: $elm$core$Maybe$Just(
									A2(
										$author$project$Morphir$Scala$AST$Ref,
										_List_fromArray(
											['org.springframework.boot.SpringApplication']),
										'run(classOf[MainApplication], args:_*)')),
								aI: _List_fromArray(
									[
										$author$project$Morphir$Scala$AST$TypeVar('App')
									]),
								aK: _List_fromArray(
									[
										$author$project$Morphir$Scala$AST$withoutAnnotation(
										$author$project$Morphir$Scala$AST$ValueDecl(
											{
												e4: _List_Nil,
												cU: $author$project$Morphir$Scala$AST$NamedMatch('metricRegistry'),
												aT: $author$project$Morphir$Scala$AST$Variable('new com.codahale.metrics.MetricRegistry'),
												$7: $elm$core$Maybe$Nothing
											}))
									]),
								e4: _List_Nil,
								cN: 'MainApplication'
							})))
				])
		};
		return _List_fromArray(
			[moduleMainApp]);
	});
var $author$project$Morphir$Scala$AST$ArgDecl = F4(
	function (modifiers, tpe, name, defaultValue) {
		return {ek: defaultValue, e4: modifiers, cN: name, gl: tpe};
	});
var $author$project$Morphir$SpringBoot$Backend$mapStatefulAppDefinition = F2(
	function (currentPackagePath, currentModulePath) {
		var scalaPackagePath = A2($author$project$Morphir$SpringBoot$Backend$getScalaPackagePath, currentPackagePath, currentModulePath).a;
		var moduleMainApp = {
			em: scalaPackagePath,
			ez: 'StatefulApp.scala',
			eM: _List_Nil,
			fs: scalaPackagePath,
			gm: _List_fromArray(
				[
					A2(
					$author$project$Morphir$Scala$AST$Documented,
					$elm$core$Maybe$Just(
						A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									'Generated based on ',
									A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', currentModulePath)
								]))),
					$author$project$Morphir$Scala$AST$withoutAnnotation(
						$author$project$Morphir$Scala$AST$Class(
							{
								cc: _List_fromArray(
									[
										_List_fromArray(
										[
											A4(
											$author$project$Morphir$Scala$AST$ArgDecl,
											_List_Nil,
											A2(
												$author$project$Morphir$Scala$AST$FunctionType,
												A2(
													$author$project$Morphir$Scala$AST$TypeApply,
													A2(
														$author$project$Morphir$Scala$AST$TypeRef,
														_List_fromArray(
															['morphir', 'sdk', 'Maybe']),
														'Maybe'),
													_List_fromArray(
														[
															$author$project$Morphir$Scala$AST$TypeVar('S')
														])),
												A2(
													$author$project$Morphir$Scala$AST$FunctionType,
													$author$project$Morphir$Scala$AST$TypeVar('C'),
													$author$project$Morphir$Scala$AST$TupleType(
														_List_fromArray(
															[
																A2(
																$author$project$Morphir$Scala$AST$TypeApply,
																A2(
																	$author$project$Morphir$Scala$AST$TypeRef,
																	_List_fromArray(
																		['morphir', 'sdk', 'Maybe']),
																	'Maybe'),
																_List_fromArray(
																	[
																		$author$project$Morphir$Scala$AST$TypeVar('S')
																	])),
																$author$project$Morphir$Scala$AST$TypeVar('E')
															])))),
											'businessLogic',
											$elm$core$Maybe$Nothing)
										])
									]),
								aI: _List_Nil,
								aK: _List_Nil,
								e4: _List_fromArray(
									[$author$project$Morphir$Scala$AST$Case]),
								cN: 'StatefulApp',
								ac: _List_fromArray(
									[
										$author$project$Morphir$Scala$AST$TypeVar('K'),
										$author$project$Morphir$Scala$AST$TypeVar('C'),
										$author$project$Morphir$Scala$AST$TypeVar('S'),
										$author$project$Morphir$Scala$AST$TypeVar('E')
									])
							})))
				])
		};
		return _List_fromArray(
			[moduleMainApp]);
	});
var $author$project$Morphir$Scala$AST$Abstract = {$: 7};
var $author$project$Morphir$Scala$AST$TypeParametrized = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $author$project$Morphir$SpringBoot$Backend$addJacksonAnnotations = function (annotatedMembers) {
	var mapMember = function (annotatedMemberTypeDecl) {
		var _v0 = annotatedMemberTypeDecl.aT;
		_v0$2:
		while (true) {
			if (_v0.$ === 3) {
				switch (_v0.a.$) {
					case 0:
						var caseClassNames = A2(
							$elm$core$List$filterMap,
							function (member) {
								if ((member.$ === 3) && (member.a.$ === 1)) {
									var _class = member.a.a;
									return $elm$core$Maybe$Just(_class.cN);
								} else {
									return $elm$core$Maybe$Nothing;
								}
							},
							A2(
								$elm$core$List$map,
								function ($) {
									return $.aT;
								},
								annotatedMembers));
						return A2(
							$author$project$Morphir$Scala$AST$Annotated,
							A2(
								$elm$core$List$append,
								_List_fromArray(
									[
										'@com.fasterxml.jackson.annotation.JsonTypeInfo(use = com.fasterxml.jackson.annotation.JsonTypeInfo.Id.NAME,' + ($author$project$Morphir$File$SourceCode$newLine + ('include = com.fasterxml.jackson.annotation.JsonTypeInfo.As.PROPERTY, property = \"type\")' + ($author$project$Morphir$File$SourceCode$newLine + ('@com.fasterxml.jackson.annotation.JsonSubTypes(Array' + ($author$project$Morphir$File$SourceCode$newLine + ('(' + ($author$project$Morphir$File$SourceCode$newLine + ($elm$core$String$concat(
										A2(
											$elm$core$List$map,
											function (name) {
												return 'new com.fasterxml.jackson.annotation.JsonSubTypes.Type(value = classOf[' + (name + ('], name = \"' + (name + ('\"),' + $author$project$Morphir$File$SourceCode$newLine))));
											},
											caseClassNames)) + '))'))))))))
									]),
								annotatedMemberTypeDecl.dN),
							annotatedMemberTypeDecl.aT);
					case 1:
						var _class = _v0.a.a;
						return A2(
							$author$project$Morphir$Scala$AST$Annotated,
							annotatedMemberTypeDecl.dN,
							$author$project$Morphir$Scala$AST$MemberTypeDecl(
								$author$project$Morphir$Scala$AST$Class(
									{
										cc: A2(
											$elm$core$List$map,
											function (args) {
												return A2(
													$elm$core$List$concatMap,
													function (cons) {
														return _List_fromArray(
															[
																{ek: cons.ek, e4: cons.e4, cN: '@scala.beans.BeanProperty ' + ('@com.fasterxml.jackson.annotation.JsonProperty (\"' + (cons.cN + ('\") ' + cons.cN))), gl: cons.gl}
															]);
													},
													args);
											},
											_class.cc),
										aI: _class.aI,
										aK: _class.aK,
										e4: _class.e4,
										cN: _class.cN,
										ac: _class.ac
									})));
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return annotatedMemberTypeDecl;
	};
	return A2($elm$core$List$map, mapMember, annotatedMembers);
};
var $author$project$Morphir$IR$Distribution$lookupTypeSpecification = F4(
	function (packageName, moduleName, localName, distribution) {
		return A2(
			$elm$core$Maybe$andThen,
			$author$project$Morphir$IR$Module$lookupTypeSpecification(localName),
			A3($author$project$Morphir$IR$Distribution$lookupModuleSpecification, packageName, moduleName, distribution));
	});
var $author$project$Morphir$SpringBoot$Backend$mapStatefulAppImplementation = F5(
	function (opt, distribution, currentPackagePath, currentModulePath, accessControlledModuleDef) {
		var statefulAppTypes = A2(
			$elm$core$List$concatMap,
			function (_v33) {
				var a = _v33.b;
				var _v34 = a.aT.fq;
				if (_v34.$ === 1) {
					var _v35 = _v34.b;
					var mod = _v35.a;
					var _package = _v35.b;
					var name = _v35.c;
					var list = _v34.c;
					var _v36 = _Utils_Tuple3(
						A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', mod),
						A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', _package),
						$author$project$Morphir$IR$Name$toTitleCase(name));
					if (((_v36.a === 'Morphir.SDK') && (_v36.b === 'StatefulApp')) && (_v36.c === 'StatefulApp')) {
						return A2($elm$core$List$map, $author$project$Morphir$Scala$Backend$mapType, list);
					} else {
						return _List_Nil;
					}
				} else {
					return _List_Nil;
				}
			},
			$elm$core$Dict$toList(accessControlledModuleDef.aT.gq));
		var typeNamesStatefulApp = function () {
			if ((((((((statefulAppTypes.b && (statefulAppTypes.a.$ === 1)) && statefulAppTypes.b.b) && (statefulAppTypes.b.a.$ === 1)) && statefulAppTypes.b.b.b) && (statefulAppTypes.b.b.a.$ === 1)) && statefulAppTypes.b.b.b.b) && (statefulAppTypes.b.b.b.a.$ === 1)) && (!statefulAppTypes.b.b.b.b.b)) {
				var _v26 = statefulAppTypes.a;
				var keyTypeName = _v26.b;
				var _v27 = statefulAppTypes.b;
				var _v28 = _v27.a;
				var commandTypeName = _v28.b;
				var _v29 = _v27.b;
				var _v30 = _v29.a;
				var stateTypeName = _v30.b;
				var _v31 = _v29.b;
				var _v32 = _v31.a;
				var eventTypeName = _v32.b;
				return _List_fromArray(
					[keyTypeName, commandTypeName, stateTypeName, eventTypeName]);
			} else {
				return _List_Nil;
			}
		}();
		var scalaPackagePath = A2($author$project$Morphir$SpringBoot$Backend$getScalaPackagePath, currentPackagePath, currentModulePath).a;
		var moduleName = A2($author$project$Morphir$SpringBoot$Backend$getScalaPackagePath, currentPackagePath, currentModulePath).b;
		var memberStatefulApp = function (name) {
			var _v24 = A2(
				$elm$core$Dict$get,
				$author$project$Morphir$IR$Name$fromString(name),
				accessControlledModuleDef.aT.gn);
			if (!_v24.$) {
				var accessControlledDocumentedTypeDef = _v24.a;
				return A4(
					$author$project$Morphir$Scala$Backend$mapTypeMember,
					currentPackagePath,
					currentModulePath,
					accessControlledModuleDef,
					_Utils_Tuple2(
						$author$project$Morphir$IR$Name$fromString(name),
						accessControlledDocumentedTypeDef));
			} else {
				return _List_Nil;
			}
		};
		var statefulAppMembers = function () {
			if ((((typeNamesStatefulApp.b && typeNamesStatefulApp.b.b) && typeNamesStatefulApp.b.b.b) && typeNamesStatefulApp.b.b.b.b) && (!typeNamesStatefulApp.b.b.b.b.b)) {
				var keyTypeName = typeNamesStatefulApp.a;
				var _v21 = typeNamesStatefulApp.b;
				var commandTypeName = _v21.a;
				var _v22 = _v21.b;
				var stateTypeName = _v22.a;
				var _v23 = _v22.b;
				var eventTypeName = _v23.a;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							memberStatefulApp(keyTypeName),
							$author$project$Morphir$SpringBoot$Backend$addJacksonAnnotations(
							memberStatefulApp(eventTypeName)),
							$author$project$Morphir$SpringBoot$Backend$addJacksonAnnotations(
							memberStatefulApp(commandTypeName)),
							memberStatefulApp(stateTypeName)
						]));
			} else {
				return _List_Nil;
			}
		}();
		var innerTypesNamesStatefulApp = $elm_community$list_extra$List$Extra$unique(
			A2(
				$elm$core$List$concatMap,
				function (name) {
					var _v12 = A4(
						$author$project$Morphir$IR$Distribution$lookupTypeSpecification,
						currentPackagePath,
						currentModulePath,
						$author$project$Morphir$IR$Name$fromString(name),
						distribution);
					_v12$2:
					while (true) {
						if (!_v12.$) {
							switch (_v12.a.$) {
								case 0:
									var _v13 = _v12.a;
									var aliasType = _v13.b;
									var _v14 = $author$project$Morphir$Scala$Backend$mapType(aliasType);
									if (_v14.$ === 1) {
										var typeName = _v14.b;
										return _List_fromArray(
											[typeName]);
									} else {
										return _List_Nil;
									}
								case 2:
									var _v15 = _v12.a;
									var constructors = _v15.b;
									return A2(
										$elm$core$List$concatMap,
										function (constructor) {
											var types = constructor.b;
											return A2(
												$elm$core$List$concatMap,
												function (_v17) {
													var consType = _v17.b;
													if (consType.$ === 1) {
														var _v19 = consType.b;
														var consTypeName = _v19.c;
														return _List_fromArray(
															[
																$author$project$Morphir$IR$Name$toTitleCase(consTypeName)
															]);
													} else {
														return _List_Nil;
													}
												},
												types);
										},
										constructors);
								default:
									break _v12$2;
							}
						} else {
							break _v12$2;
						}
					}
					return _List_Nil;
				},
				typeNamesStatefulApp));
		var innerMembers = A2(
			$elm$core$List$concatMap,
			function (_v11) {
				var typeName = _v11.a;
				var accessControlledDocumentedTypeDef = _v11.b;
				return A2(
					$elm$core$List$member,
					$author$project$Morphir$IR$Name$toTitleCase(typeName),
					innerTypesNamesStatefulApp) ? A4(
					$author$project$Morphir$Scala$Backend$mapTypeMember,
					currentPackagePath,
					currentModulePath,
					accessControlledModuleDef,
					_Utils_Tuple2(typeName, accessControlledDocumentedTypeDef)) : _List_Nil;
			},
			$elm$core$Dict$toList(accessControlledModuleDef.aT.gn));
		var functionName = function () {
			var _v3 = accessControlledModuleDef.b0;
			if (!_v3) {
				var _v4 = accessControlledModuleDef.aT;
				var types = _v4.gn;
				var values = _v4.gq;
				var _v5 = A2(
					$elm$core$Dict$get,
					$author$project$Morphir$IR$Name$fromString('app'),
					values);
				if (!_v5.$) {
					var acsCtrlValueDef = _v5.a;
					var _v6 = acsCtrlValueDef.b0;
					if (!_v6) {
						var _v7 = acsCtrlValueDef.aT.dV;
						if (((_v7.$ === 9) && (_v7.b.$ === 1)) && (_v7.c.$ === 6)) {
							var _v8 = _v7.b;
							var _v9 = _v7.c;
							var _v10 = _v9.b;
							var name = _v10.c;
							return name;
						} else {
							return _List_Nil;
						}
					} else {
						return _List_Nil;
					}
				} else {
					return _List_Nil;
				}
			} else {
				return _List_Nil;
			}
		}();
		var stateFulImplAdapter = {
			em: scalaPackagePath,
			ez: $author$project$Morphir$IR$Name$toTitleCase(moduleName) + ('SpringBoot' + '.scala'),
			eM: _List_Nil,
			fs: scalaPackagePath,
			gm: _List_fromArray(
				[
					A2(
					$author$project$Morphir$Scala$AST$Documented,
					$elm$core$Maybe$Just(
						A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									'Generated based on ',
									A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', currentModulePath)
								]))),
					A2(
						$author$project$Morphir$Scala$AST$Annotated,
						_List_fromArray(
							['@org.springframework.web.bind.annotation.RestController']),
						$author$project$Morphir$Scala$AST$Class(
							{
								cc: _List_Nil,
								aI: _List_fromArray(
									[
										A3(
										$author$project$Morphir$Scala$AST$TypeParametrized,
										$author$project$Morphir$Scala$AST$TypeVar('SpringBootStatefulAppAdapter'),
										statefulAppTypes,
										$author$project$Morphir$Scala$AST$TypeVar(
											'StatefulApp (' + ($author$project$Morphir$File$SourceCode$dotSep(scalaPackagePath) + ('.' + ($author$project$Morphir$IR$Name$toTitleCase(moduleName) + ('.' + (A2(
												$elm$core$String$join,
												'',
												$author$project$Morphir$IR$Name$toList(functionName)) + ' _ )')))))))
									]),
								aK: _List_Nil,
								e4: _List_Nil,
								cN: $author$project$Morphir$IR$Name$toTitleCase(moduleName) + 'SpringBoot',
								ac: _List_Nil
							})))
				])
		};
		var functionMembers = A2(
			$elm$core$List$map,
			$author$project$Morphir$Scala$AST$withoutAnnotation,
			A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var valueName = _v0.a;
					var accessControlledValueDef = _v0.b;
					return _Utils_eq(
						$author$project$Morphir$IR$Name$toTitleCase(valueName),
						$author$project$Morphir$IR$Name$toTitleCase(functionName)) ? _List_fromArray(
						[
							$author$project$Morphir$Scala$AST$FunctionDecl(
							{
								dP: $elm$core$List$isEmpty(accessControlledValueDef.aT.a3) ? _List_Nil : A2(
									$elm$core$List$map,
									function (_v1) {
										var argName = _v1.a;
										var argType = _v1.c;
										return _List_fromArray(
											[
												{
												ek: $elm$core$Maybe$Nothing,
												e4: _List_Nil,
												cN: $author$project$Morphir$IR$Name$toCamelCase(argName),
												gl: $author$project$Morphir$Scala$Backend$mapType(argType)
											}
											]);
									},
									accessControlledValueDef.aT.a3),
								dV: $elm$core$Maybe$Just(
									A2($author$project$Morphir$Scala$Backend$mapFunctionBody, distribution, accessControlledValueDef.aT.dV)),
								e4: function () {
									var _v2 = accessControlledValueDef.b0;
									if (!_v2) {
										return _List_Nil;
									} else {
										return _List_fromArray(
											[
												$author$project$Morphir$Scala$AST$Private($elm$core$Maybe$Nothing)
											]);
									}
								}(),
								cN: $author$project$Morphir$IR$Name$toCamelCase(valueName),
								aO: $elm$core$Maybe$Just(
									$author$project$Morphir$Scala$Backend$mapType(accessControlledValueDef.aT.fq)),
								ac: _List_Nil
							})
						]) : _List_Nil;
				},
				$elm$core$Dict$toList(accessControlledModuleDef.aT.gq)));
		var statefulModule = {
			em: scalaPackagePath,
			ez: $author$project$Morphir$IR$Name$toTitleCase(moduleName) + '.scala',
			eM: _List_Nil,
			fs: scalaPackagePath,
			gm: _List_fromArray(
				[
					A2(
					$author$project$Morphir$Scala$AST$Documented,
					$elm$core$Maybe$Just(
						A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									'Generated based on ',
									A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', currentModulePath)
								]))),
					A2(
						$author$project$Morphir$Scala$AST$Annotated,
						_List_Nil,
						$author$project$Morphir$Scala$AST$Object(
							{
								dV: $elm$core$Maybe$Nothing,
								aI: _List_Nil,
								aK: A2(
									$elm$core$List$append,
									innerMembers,
									A2($elm$core$List$append, statefulAppMembers, functionMembers)),
								e4: _List_Nil,
								cN: $author$project$Morphir$IR$Name$toTitleCase(moduleName)
							})))
				])
		};
		var adapterAbstractModule = {
			em: scalaPackagePath,
			ez: 'SpringBootStatefulAppAdapter.scala',
			eM: _List_Nil,
			fs: scalaPackagePath,
			gm: _List_fromArray(
				[
					A2(
					$author$project$Morphir$Scala$AST$Documented,
					$elm$core$Maybe$Just(
						A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									'Generated based on ',
									A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toTitleCase, '.', currentModulePath)
								]))),
					$author$project$Morphir$Scala$AST$withoutAnnotation(
						$author$project$Morphir$Scala$AST$Class(
							{
								cc: _List_fromArray(
									[
										_List_fromArray(
										[
											A4(
											$author$project$Morphir$Scala$AST$ArgDecl,
											_List_Nil,
											A2(
												$author$project$Morphir$Scala$AST$TypeApply,
												$author$project$Morphir$Scala$AST$TypeVar(
													$author$project$Morphir$File$SourceCode$dotSep(scalaPackagePath) + '.StatefulApp'),
												_List_fromArray(
													[
														$author$project$Morphir$Scala$AST$TypeVar('K'),
														$author$project$Morphir$Scala$AST$TypeVar('C'),
														$author$project$Morphir$Scala$AST$TypeVar('S'),
														$author$project$Morphir$Scala$AST$TypeVar('E')
													])),
											'statefulApp',
											$elm$core$Maybe$Nothing)
										])
									]),
								aI: _List_Nil,
								aK: _List_fromArray(
									[
										$author$project$Morphir$Scala$AST$withoutAnnotation(
										$author$project$Morphir$Scala$AST$ValueDecl(
											{
												e4: _List_Nil,
												cU: $author$project$Morphir$Scala$AST$NamedMatch('requests'),
												aT: $author$project$Morphir$Scala$AST$Variable(
													$author$project$Morphir$File$SourceCode$dotSep(scalaPackagePath) + '.MainApplication.metricRegistry.meter(\"statefulAppRequests\")'),
												$7: $elm$core$Maybe$Nothing
											})),
										A2(
										$author$project$Morphir$Scala$AST$Annotated,
										_List_fromArray(
											['@org.springframework.web.bind.annotation.PostMapping(value= Array(\"/v1.0/command\"), consumes = Array(org.springframework.http.MediaType.APPLICATION_JSON_VALUE), produces = Array(\"application/json\"))']),
										$author$project$Morphir$Scala$AST$FunctionDecl(
											{
												dP: _List_fromArray(
													[
														_List_fromArray(
														[
															A4(
															$author$project$Morphir$Scala$AST$ArgDecl,
															_List_Nil,
															$author$project$Morphir$Scala$AST$TypeVar('C'),
															'@org.springframework.web.bind.annotation.RequestBody command',
															$elm$core$Maybe$Nothing)
														])
													]),
												dV: $elm$core$Maybe$Just(
													$author$project$Morphir$Scala$AST$Variable('{requests.mark' + ($author$project$Morphir$File$SourceCode$newLine + 'process(command, None)._2}'))),
												e4: _List_Nil,
												cN: 'entryPoint',
												aO: $elm$core$Maybe$Just(
													$author$project$Morphir$Scala$AST$TypeVar('E')),
												ac: _List_Nil
											})),
										$author$project$Morphir$Scala$AST$withoutAnnotation(
										$author$project$Morphir$Scala$AST$FunctionDecl(
											{
												dP: _List_fromArray(
													[
														_List_fromArray(
														[
															A4(
															$author$project$Morphir$Scala$AST$ArgDecl,
															_List_Nil,
															$author$project$Morphir$Scala$AST$TypeVar('C'),
															'command',
															$elm$core$Maybe$Nothing),
															A4(
															$author$project$Morphir$Scala$AST$ArgDecl,
															_List_Nil,
															A2(
																$author$project$Morphir$Scala$AST$TypeApply,
																$author$project$Morphir$Scala$AST$TypeVar('Option'),
																_List_fromArray(
																	[
																		$author$project$Morphir$Scala$AST$TypeVar('S')
																	])),
															'state',
															$elm$core$Maybe$Nothing)
														])
													]),
												dV: $elm$core$Maybe$Just(
													A2(
														$author$project$Morphir$Scala$AST$Apply,
														A2(
															$author$project$Morphir$Scala$AST$Apply,
															A2(
																$author$project$Morphir$Scala$AST$Ref,
																_List_fromArray(
																	['statefulApp']),
																'businessLogic'),
															_List_fromArray(
																[
																	A2(
																	$author$project$Morphir$Scala$AST$ArgValue,
																	$elm$core$Maybe$Nothing,
																	$author$project$Morphir$Scala$AST$Variable('state'))
																])),
														_List_fromArray(
															[
																A2(
																$author$project$Morphir$Scala$AST$ArgValue,
																$elm$core$Maybe$Nothing,
																$author$project$Morphir$Scala$AST$Variable('command'))
															]))),
												e4: _List_Nil,
												cN: 'process',
												aO: $elm$core$Maybe$Just(
													$author$project$Morphir$Scala$AST$TupleType(
														_List_fromArray(
															[
																$author$project$Morphir$Scala$AST$TypeVar('morphir.sdk.Maybe.Maybe[S]'),
																$author$project$Morphir$Scala$AST$TypeVar('E')
															]))),
												ac: _List_Nil
											})),
										A2(
										$author$project$Morphir$Scala$AST$Annotated,
										_List_fromArray(
											['@springfox.documentation.annotations.ApiIgnore' + ($author$project$Morphir$File$SourceCode$newLine + '@org.springframework.web.bind.annotation.RequestMapping(value = Array(\"/\"))')]),
										$author$project$Morphir$Scala$AST$FunctionDecl(
											{
												dP: _List_Nil,
												dV: $elm$core$Maybe$Just(
													$author$project$Morphir$Scala$AST$Variable('new org.springframework.web.servlet.ModelAndView(\"redirect:/swagger-ui/index.html\")')),
												e4: _List_Nil,
												cN: 'home',
												aO: $elm$core$Maybe$Nothing,
												ac: _List_Nil
											}))
									]),
								e4: _List_fromArray(
									[$author$project$Morphir$Scala$AST$Abstract]),
								cN: 'SpringBootStatefulAppAdapter',
								ac: _List_fromArray(
									[
										$author$project$Morphir$Scala$AST$TypeVar('K'),
										$author$project$Morphir$Scala$AST$TypeVar('C'),
										$author$project$Morphir$Scala$AST$TypeVar('S'),
										$author$project$Morphir$Scala$AST$TypeVar('E')
									])
							})))
				])
		};
		return _List_fromArray(
			[stateFulImplAdapter, statefulModule, adapterAbstractModule]);
	});
var $author$project$Morphir$SpringBoot$Backend$mapPackageDefinition = F4(
	function (opt, distribution, packagePath, packageDef) {
		return $elm$core$Dict$fromList(
			A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var modulePath = _v0.a;
					var moduleImpl = _v0.b;
					return A2(
						$elm$core$List$map,
						function (compilationUnit) {
							var fileContent = A2(
								$author$project$Morphir$Scala$PrettyPrinter$mapCompilationUnit,
								A2($author$project$Morphir$Scala$PrettyPrinter$Options, 2, 80),
								compilationUnit);
							return _Utils_Tuple2(
								_Utils_Tuple2(
									A2(
										$elm$core$List$append,
										_List_fromArray(
											['src', 'main', 'java']),
										compilationUnit.em),
									compilationUnit.ez),
								fileContent);
						},
						A2(
							$elm$core$List$append,
							A2($author$project$Morphir$SpringBoot$Backend$mapStatefulAppDefinition, packagePath, modulePath),
							A2(
								$elm$core$List$append,
								A2($author$project$Morphir$SpringBoot$Backend$mapMainApp, packagePath, modulePath),
								A5($author$project$Morphir$SpringBoot$Backend$mapStatefulAppImplementation, opt, distribution, packagePath, modulePath, moduleImpl))));
				},
				$elm$core$Dict$toList(packageDef.e7)));
	});
var $author$project$Morphir$SpringBoot$Backend$mapDistribution = F2(
	function (opt, distro) {
		var packagePath = distro.a;
		var dependencies = distro.b;
		var packageDef = distro.c;
		return A4($author$project$Morphir$SpringBoot$Backend$mapPackageDefinition, opt, distro, packagePath, packageDef);
	});
var $author$project$Morphir$Elm$Target$mapDistribution = F2(
	function (back, dist) {
		switch (back.$) {
			case 1:
				var options = back.a;
				return A2($author$project$Morphir$SpringBoot$Backend$mapDistribution, options, dist);
			case 2:
				var options = back.a;
				return A2($author$project$Morphir$Graph$TriplesBackend$mapDistribution, options, dist);
			case 3:
				var options = back.a;
				return A2($author$project$Morphir$Graph$CypherBackend$mapDistribution, options, dist);
			default:
				var options = back.a;
				return A2($author$project$Morphir$Scala$Backend$mapDistribution, options, dist);
		}
	});
var $author$project$Morphir$Compiler$ErrorAcrossSourceFiles = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Elm$Frontend$Resolve$errorToMessage = function (error) {
	switch (error.$) {
		case 0:
			var trace = error.a;
			var target = error.b;
			var localName = error.c;
			return $elm$core$String$concat(
				_List_fromArray(
					['Could not find local name \'', localName, '\'']));
		case 1:
			var nameType = error.a;
			var packagePath = error.b;
			var modulePath = error.c;
			var localName = error.d;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Could not find name \'',
						$author$project$Morphir$IR$Name$toCamelCase(localName),
						'\' in module \'',
						A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', modulePath),
						'\' in package \'',
						A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', packagePath),
						'\''
					]));
		case 2:
			var packageAndModulePath = error.a;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Could not find module \'',
						A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', packageAndModulePath)
					]));
		case 3:
			var localName = error.a;
			var modulePaths = error.b;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Ambiguous local name imports. The local name `',
						localName,
						'\' is imported from multiple modules: ',
						A2(
						$elm$core$String$join,
						', ',
						A2(
							$elm$core$List$map,
							function (_v1) {
								var packagePath = _v1.a;
								var modulePath = _v1.b;
								return A2(
									$elm$core$String$join,
									'.',
									_List_fromArray(
										[
											A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', packagePath),
											A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', modulePath)
										]));
							},
							modulePaths))
					]));
		default:
			var packageAndModulePath = error.a;
			var matchingPaths = error.b;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Ambiguous module imports. The module name `',
						A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', packageAndModulePath),
						'\' matches multiple modules: ',
						A2(
						$elm$core$String$join,
						', ',
						A2(
							$elm$core$List$map,
							function (_v2) {
								var packagePath = _v2.a;
								var modulePath = _v2.b;
								return A2(
									$elm$core$String$join,
									'.',
									_List_fromArray(
										[
											A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', packagePath),
											A3($author$project$Morphir$IR$Path$toString, $author$project$Morphir$IR$Name$toCamelCase, '.', modulePath)
										]));
							},
							matchingPaths))
					]));
	}
};
var $author$project$Morphir$Graph$nodeLabels = function (_v0) {
	var startNodeList = _v0;
	return $elm$core$Set$fromList(
		A2(
			$elm$core$List$concatMap,
			function (_v1) {
				var startLabel = _v1.b;
				var endLabels = _v1.c;
				return A2(
					$elm$core$List$cons,
					startLabel,
					$elm$core$Set$toList(endLabels));
			},
			startNodeList));
};
var $author$project$Morphir$Elm$Frontend$CyclicModules = function (a) {
	return {$: 1, a: a};
};
var $author$project$Morphir$Elm$Frontend$ParseError = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Morphir$Elm$Frontend$ParsedFile = F2(
	function (sourceFile, rawFile) {
		return {aN: rawFile, bT: sourceFile};
	});
var $author$project$Morphir$Graph$Graph = $elm$core$Basics$identity;
var $author$project$Morphir$Graph$fromList = function (list) {
	return A2(
		$elm$core$List$map,
		function (_v0) {
			var node = _v0.a;
			var fromKey = _v0.b;
			var toKeys = _v0.c;
			return _Utils_Tuple3(
				node,
				fromKey,
				$elm$core$Set$fromList(toKeys));
		},
		list);
};
var $stil4m$elm_syntax$Elm$Syntax$Node$value = function (_v0) {
	var v = _v0.b;
	return v;
};
var $stil4m$elm_syntax$Elm$RawFile$imports = function (_v0) {
	var file = _v0;
	return A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, file.eM);
};
var $author$project$Morphir$Graph$isEmpty = function (_v0) {
	var edges = _v0;
	return $elm$core$List$isEmpty(edges);
};
var $elm$core$Set$foldl = F3(
	function (func, initialState, _v0) {
		var dict = _v0;
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (key, _v1, state) {
					return A2(func, key, state);
				}),
			initialState,
			dict);
	});
var $elm$core$Set$map = F2(
	function (func, set) {
		return $elm$core$Set$fromList(
			A3(
				$elm$core$Set$foldl,
				F2(
					function (x, xs) {
						return A2(
							$elm$core$List$cons,
							func(x),
							xs);
					}),
				_List_Nil,
				set));
	});
var $author$project$Morphir$Elm$Frontend$ProcessedFile = F2(
	function (parsedFile, file) {
		return {az: file, bO: parsedFile};
	});
var $stil4m$elm_syntax$Elm$Processing$ProcessContext = $elm$core$Basics$identity;
var $stil4m$elm_syntax$Elm$Interface$CustomType = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Interface$Function = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Interface$ifCustomType = F2(
	function (f, i) {
		if (i.$ === 1) {
			var t = i.a;
			return f(t);
		} else {
			return i;
		}
	});
var $stil4m$elm_syntax$Elm$Interface$lookupForDefinition = function (key) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$filter(
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Tuple$first,
				$elm$core$Basics$eq(key))),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$head,
			$elm$core$Maybe$map($elm$core$Tuple$second)));
};
var $stil4m$elm_syntax$Elm$Interface$buildInterfaceFromExplicit = F2(
	function (x, fileDefinitionList) {
		return A2(
			$elm$core$List$filterMap,
			function (_v0) {
				var expose = _v0.b;
				switch (expose.$) {
					case 0:
						var k = expose.a;
						return A2($stil4m$elm_syntax$Elm$Interface$lookupForDefinition, k, fileDefinitionList);
					case 2:
						var s = expose.a;
						return A2(
							$elm$core$Maybe$map,
							$stil4m$elm_syntax$Elm$Interface$ifCustomType(
								function (_v2) {
									var name = _v2.a;
									return $stil4m$elm_syntax$Elm$Interface$CustomType(
										_Utils_Tuple2(name, _List_Nil));
								}),
							A2($stil4m$elm_syntax$Elm$Interface$lookupForDefinition, s, fileDefinitionList));
					case 1:
						var s = expose.a;
						return $elm$core$Maybe$Just(
							$stil4m$elm_syntax$Elm$Interface$Function(s));
					default:
						var exposedType = expose.a;
						var _v3 = exposedType.fo;
						if (_v3.$ === 1) {
							return $elm$core$Maybe$Just(
								$stil4m$elm_syntax$Elm$Interface$CustomType(
									_Utils_Tuple2(exposedType.cN, _List_Nil)));
						} else {
							return A2($stil4m$elm_syntax$Elm$Interface$lookupForDefinition, exposedType.cN, fileDefinitionList);
						}
				}
			},
			x);
	});
var $stil4m$elm_syntax$Elm$Syntax$Module$exposingList = function (m) {
	switch (m.$) {
		case 0:
			var x = m.a;
			return $stil4m$elm_syntax$Elm$Syntax$Node$value(x.ew);
		case 1:
			var x = m.a;
			return $stil4m$elm_syntax$Elm$Syntax$Node$value(x.ew);
		default:
			var x = m.a;
			return $stil4m$elm_syntax$Elm$Syntax$Node$value(x.ew);
	}
};
var $stil4m$elm_syntax$Elm$Interface$Alias = function (a) {
	return {$: 2, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Infix$Left = 0;
var $stil4m$elm_syntax$Elm$Interface$Operator = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Interface$fileToDefinitions = function (file) {
	var getValidOperatorInterface = F2(
		function (t1, t2) {
			var _v6 = _Utils_Tuple2(t1, t2);
			if ((_v6.a.$ === 3) && (_v6.b.$ === 3)) {
				var x = _v6.a.a;
				var y = _v6.b.a;
				return (($stil4m$elm_syntax$Elm$Syntax$Node$value(x.q) === 5) && (!$stil4m$elm_syntax$Elm$Syntax$Node$value(x.m))) ? $elm$core$Maybe$Just(
					$stil4m$elm_syntax$Elm$Interface$Operator(y)) : $elm$core$Maybe$Just(
					$stil4m$elm_syntax$Elm$Interface$Operator(x));
			} else {
				return $elm$core$Maybe$Nothing;
			}
		});
	var resolveGroup = function (g) {
		if (!g.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!g.b.b) {
				var x = g.a;
				return $elm$core$Maybe$Just(x);
			} else {
				if (!g.b.b.b) {
					var _v3 = g.a;
					var n1 = _v3.a;
					var t1 = _v3.b;
					var _v4 = g.b;
					var _v5 = _v4.a;
					var t2 = _v5.b;
					return A2(
						$elm$core$Maybe$map,
						function (a) {
							return _Utils_Tuple2(n1, a);
						},
						A2(getValidOperatorInterface, t1, t2));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	};
	var allDeclarations = A2(
		$elm$core$List$filterMap,
		function (_v0) {
			var decl = _v0.b;
			switch (decl.$) {
				case 2:
					var t = decl.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							$stil4m$elm_syntax$Elm$Syntax$Node$value(t.cN),
							$stil4m$elm_syntax$Elm$Interface$CustomType(
								_Utils_Tuple2(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(t.cN),
									A2(
										$elm$core$List$map,
										A2(
											$elm$core$Basics$composeR,
											$stil4m$elm_syntax$Elm$Syntax$Node$value,
											A2(
												$elm$core$Basics$composeR,
												function ($) {
													return $.cN;
												},
												$stil4m$elm_syntax$Elm$Syntax$Node$value)),
										t.ed)))));
				case 1:
					var a = decl.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							$stil4m$elm_syntax$Elm$Syntax$Node$value(a.cN),
							$stil4m$elm_syntax$Elm$Interface$Alias(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(a.cN))));
				case 3:
					var p = decl.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							$stil4m$elm_syntax$Elm$Syntax$Node$value(p.cN),
							$stil4m$elm_syntax$Elm$Interface$Function(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(p.cN))));
				case 0:
					var f = decl.a;
					var declaration = $stil4m$elm_syntax$Elm$Syntax$Node$value(f.am);
					var name = $stil4m$elm_syntax$Elm$Syntax$Node$value(declaration.cN);
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							name,
							$stil4m$elm_syntax$Elm$Interface$Function(name)));
				case 4:
					var i = decl.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							$stil4m$elm_syntax$Elm$Syntax$Node$value(i.o),
							$stil4m$elm_syntax$Elm$Interface$Operator(i)));
				default:
					return $elm$core$Maybe$Nothing;
			}
		},
		file.a0);
	return A2(
		$elm$core$List$filterMap,
		A2($elm$core$Basics$composeR, $elm$core$Tuple$second, resolveGroup),
		A2(
			$elm$core$List$map,
			function (x) {
				return _Utils_Tuple2(
					x,
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$first,
							$elm$core$Basics$eq(x)),
						allDeclarations));
			},
			$elm_community$list_extra$List$Extra$unique(
				A2($elm$core$List$map, $elm$core$Tuple$first, allDeclarations))));
};
var $stil4m$elm_syntax$Elm$Interface$build = function (_v0) {
	var file = _v0;
	var fileDefinitionList = $stil4m$elm_syntax$Elm$Interface$fileToDefinitions(file);
	var _v1 = $stil4m$elm_syntax$Elm$Syntax$Module$exposingList(
		$stil4m$elm_syntax$Elm$Syntax$Node$value(file.cL));
	if (_v1.$ === 1) {
		var x = _v1.a;
		return A2($stil4m$elm_syntax$Elm$Interface$buildInterfaceFromExplicit, x, fileDefinitionList);
	} else {
		return A2($elm$core$List$map, $elm$core$Tuple$second, fileDefinitionList);
	}
};
var $stil4m$elm_syntax$Elm$Syntax$Module$moduleName = function (m) {
	switch (m.$) {
		case 0:
			var x = m.a;
			return $stil4m$elm_syntax$Elm$Syntax$Node$value(x.bs);
		case 1:
			var x = m.a;
			return $stil4m$elm_syntax$Elm$Syntax$Node$value(x.bs);
		default:
			var x = m.a;
			return $stil4m$elm_syntax$Elm$Syntax$Node$value(x.bs);
	}
};
var $stil4m$elm_syntax$Elm$RawFile$moduleName = function (_v0) {
	var file = _v0;
	return $stil4m$elm_syntax$Elm$Syntax$Module$moduleName(
		$stil4m$elm_syntax$Elm$Syntax$Node$value(file.cL));
};
var $stil4m$elm_syntax$Elm$Processing$entryFromRawFile = function (rawFile) {
	return _Utils_Tuple2(
		$stil4m$elm_syntax$Elm$RawFile$moduleName(rawFile),
		$stil4m$elm_syntax$Elm$Interface$build(rawFile));
};
var $stil4m$elm_syntax$Elm$Processing$addFile = F2(
	function (file, _v0) {
		var x = _v0;
		var _v1 = $stil4m$elm_syntax$Elm$Processing$entryFromRawFile(file);
		var k = _v1.a;
		var v = _v1.b;
		return A3($elm$core$Dict$insert, k, v, x);
	});
var $stil4m$elm_syntax$Elm$Processing$init = $elm$core$Dict$empty;
var $author$project$Morphir$Elm$Frontend$Resolve$Context = F6(
	function (dependencies, currentPackagePath, currentPackageModules, explicitImports, currentModulePath, moduleDef) {
		return {cd: currentModulePath, ce: currentPackageModules, bg: currentPackagePath, ch: dependencies, cm: explicitImports, a5: moduleDef};
	});
var $author$project$Morphir$Elm$Frontend$Resolve$AmbiguousImports = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindLocalName = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindModule = function (a) {
	return {$: 2, a: a};
};
var $author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindNameInModule = F4(
	function (a, b, c, d) {
		return {$: 1, a: a, b: b, c: c, d: d};
	});
var $author$project$Morphir$Elm$Frontend$Resolve$Ctor = 1;
var $author$project$Morphir$Elm$Frontend$Resolve$ModuleResolver = F3(
	function (resolveType, resolveCtor, resolveValue) {
		return {c5: resolveCtor, fz: resolveType, fA: resolveValue};
	});
var $author$project$Morphir$Elm$Frontend$Resolve$ResolveTarget = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$Morphir$Elm$Frontend$Resolve$ScannedLocalNames = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Morphir$Elm$Frontend$Resolve$Type = 0;
var $author$project$Morphir$Elm$Frontend$Resolve$Value = 2;
var $author$project$Morphir$Elm$Frontend$Resolve$ImportedNames = F3(
	function (typeNames, ctorNames, valueNames) {
		return {H: ctorNames, W: typeNames, X: valueNames};
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Morphir$Elm$Frontend$Resolve$collectImportedNames = F2(
	function (getModulesExposedNames, imports) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (nextImport, importedNamesSoFar) {
					return A2(
						$elm$core$Result$andThen,
						function (_v0) {
							var importPackagePath = _v0.a;
							var importModulePath = _v0.b;
							var exposedLocalNames = _v0.c;
							var appendValue = F2(
								function (key, value) {
									return A2(
										$elm$core$Dict$update,
										key,
										function (currentValue) {
											if (!currentValue.$) {
												var values = currentValue.a;
												return $elm$core$Maybe$Just(
													A2(
														$elm$core$List$append,
														values,
														_List_fromArray(
															[value])));
											} else {
												return $elm$core$Maybe$Just(
													_List_fromArray(
														[value]));
											}
										});
								});
							var addValueName = F2(
								function (localName, importedNames) {
									return _Utils_update(
										importedNames,
										{
											X: A3(
												appendValue,
												localName,
												_Utils_Tuple2(importPackagePath, importModulePath),
												importedNames.X)
										});
								});
							var addTypeName = F2(
								function (localName, importedNames) {
									return _Utils_update(
										importedNames,
										{
											W: A3(
												appendValue,
												localName,
												_Utils_Tuple2(importPackagePath, importModulePath),
												importedNames.W)
										});
								});
							var addNames = F3(
								function (addName, localNames, importedNames) {
									return A3($elm$core$List$foldl, addName, importedNames, localNames);
								});
							var addCtorName = F2(
								function (localName, importedNames) {
									return _Utils_update(
										importedNames,
										{
											H: A3(
												appendValue,
												localName,
												_Utils_Tuple2(importPackagePath, importModulePath),
												importedNames.H)
										});
								});
							var _v1 = nextImport.ew;
							if (!_v1.$) {
								var _v2 = _v1.a;
								var expose = _v2.b;
								if (expose.$ === 1) {
									var exposeList = expose.a;
									return A3(
										$elm$core$List$foldl,
										F2(
											function (_v4, explicitImportedNamesSoFar) {
												var nextTopLevelExpose = _v4.b;
												switch (nextTopLevelExpose.$) {
													case 0:
														return explicitImportedNamesSoFar;
													case 1:
														var sourceName = nextTopLevelExpose.a;
														return A2(
															$elm$core$Result$map,
															addValueName(
																$author$project$Morphir$IR$Name$fromString(sourceName)),
															explicitImportedNamesSoFar);
													case 2:
														var sourceName = nextTopLevelExpose.a;
														return A2(
															$elm$core$Result$map,
															A2(
																addNames,
																addCtorName,
																function () {
																	var _v6 = A2(
																		$elm$core$Dict$get,
																		$author$project$Morphir$IR$Name$fromString(sourceName),
																		exposedLocalNames.H);
																	if (((!_v6.$) && _v6.a.b) && (!_v6.a.b.b)) {
																		var _v7 = _v6.a;
																		var ctorName = _v7.a;
																		return _Utils_eq(
																			$author$project$Morphir$IR$Name$fromString(sourceName),
																			ctorName) ? _List_fromArray(
																			[ctorName]) : _List_Nil;
																	} else {
																		return _List_Nil;
																	}
																}()),
															A2(
																$elm$core$Result$map,
																addTypeName(
																	$author$project$Morphir$IR$Name$fromString(sourceName)),
																explicitImportedNamesSoFar));
													default:
														var exposedType = nextTopLevelExpose.a;
														var _v8 = exposedType.fo;
														if (!_v8.$) {
															return A2(
																$elm$core$Result$map,
																A2(
																	addNames,
																	addCtorName,
																	A2(
																		$elm$core$Maybe$withDefault,
																		_List_Nil,
																		A2(
																			$elm$core$Dict$get,
																			$author$project$Morphir$IR$Name$fromString(exposedType.cN),
																			exposedLocalNames.H))),
																A2(
																	$elm$core$Result$map,
																	addTypeName(
																		$author$project$Morphir$IR$Name$fromString(exposedType.cN)),
																	explicitImportedNamesSoFar));
														} else {
															return A2(
																$elm$core$Result$map,
																addTypeName(
																	$author$project$Morphir$IR$Name$fromString(exposedType.cN)),
																explicitImportedNamesSoFar);
														}
												}
											}),
										importedNamesSoFar,
										exposeList);
								} else {
									return A2(
										$elm$core$Result$map,
										A2(addNames, addValueName, exposedLocalNames.X),
										A2(
											$elm$core$Result$map,
											A2(
												addNames,
												addCtorName,
												$elm$core$List$concat(
													$elm$core$Dict$values(exposedLocalNames.H))),
											A2(
												$elm$core$Result$map,
												A2(addNames, addTypeName, exposedLocalNames.W),
												importedNamesSoFar)));
								}
							} else {
								return importedNamesSoFar;
							}
						},
						getModulesExposedNames(
							A2(
								$elm$core$List$map,
								$author$project$Morphir$IR$Name$fromString,
								$stil4m$elm_syntax$Elm$Syntax$Node$value(nextImport.bs))));
				}),
			$elm$core$Result$Ok(
				A3($author$project$Morphir$Elm$Frontend$Resolve$ImportedNames, $elm$core$Dict$empty, $elm$core$Dict$empty, $elm$core$Dict$empty)),
			imports);
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$All = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType = F2(
	function (name, open) {
		return {cN: name, fo: open};
	});
var $stil4m$elm_syntax$Elm$Syntax$Import$Import = F3(
	function (moduleName, moduleAlias, exposingList) {
		return {ew: exposingList, e5: moduleAlias, bs: moduleName};
	});
var $stil4m$elm_syntax$Elm$Syntax$Node$Node = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose = function (a) {
	return {$: 2, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange = {
	eq: {ec: 0, fD: 0},
	fT: {ec: 0, fD: 0}
};
var $author$project$Morphir$Elm$Frontend$Resolve$defaultImports = function () {
	var er = $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange;
	var en = function (a) {
		return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, a);
	};
	return _List_fromArray(
		[
			A3(
			$stil4m$elm_syntax$Elm$Syntax$Import$Import,
			en(
				_List_fromArray(
					['Basics'])),
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Just(
				en(
					$stil4m$elm_syntax$Elm$Syntax$Exposing$All($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange)))),
			A3(
			$stil4m$elm_syntax$Elm$Syntax$Import$Import,
			en(
				_List_fromArray(
					['List'])),
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Just(
				en(
					$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
						_List_fromArray(
							[
								en(
								$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose('List'))
							]))))),
			A3(
			$stil4m$elm_syntax$Elm$Syntax$Import$Import,
			en(
				_List_fromArray(
					['Maybe'])),
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Just(
				en(
					$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
						_List_fromArray(
							[
								en(
								$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
									A2(
										$stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType,
										'Maybe',
										$elm$core$Maybe$Just(er))))
							]))))),
			A3(
			$stil4m$elm_syntax$Elm$Syntax$Import$Import,
			en(
				_List_fromArray(
					['Result'])),
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Just(
				en(
					$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
						_List_fromArray(
							[
								en(
								$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
									A2(
										$stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType,
										'Result',
										$elm$core$Maybe$Just(er))))
							]))))),
			A3(
			$stil4m$elm_syntax$Elm$Syntax$Import$Import,
			en(
				_List_fromArray(
					['String'])),
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Just(
				en(
					$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
						_List_fromArray(
							[
								en(
								$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose('String'))
							]))))),
			A3(
			$stil4m$elm_syntax$Elm$Syntax$Import$Import,
			en(
				_List_fromArray(
					['Char'])),
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Just(
				en(
					$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
						_List_fromArray(
							[
								en(
								$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose('Char'))
							]))))),
			A3(
			$stil4m$elm_syntax$Elm$Syntax$Import$Import,
			en(
				_List_fromArray(
					['Tuple'])),
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Nothing)
		]);
}();
var $author$project$Morphir$Elm$Frontend$Resolve$isAmongLocalNames = F3(
	function (nameType, localName, localNames) {
		switch (nameType) {
			case 0:
				return A2($elm$core$List$member, localName, localNames.W);
			case 1:
				return A2(
					$elm$core$List$member,
					localName,
					$elm$core$List$concat(
						$elm$core$Dict$values(localNames.H)));
			default:
				return A2($elm$core$List$member, localName, localNames.X);
		}
	});
var $author$project$Morphir$ListOfResults$liftFirstError = function (results) {
	var _v0 = $author$project$Morphir$ListOfResults$liftAllErrors(results);
	if (!_v0.$) {
		var a = _v0.a;
		return $elm$core$Result$Ok(a);
	} else {
		var errors = _v0.a;
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$core$Result$Ok(_List_Nil),
			A2(
				$elm$core$Maybe$map,
				$elm$core$Result$Err,
				$elm$core$List$head(errors)));
	}
};
var $author$project$Morphir$Elm$Frontend$Resolve$AmbiguousModulePath = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $author$project$Morphir$IR$Path$isPrefixOf = F2(
	function (prefix, path) {
		isPrefixOf:
		while (true) {
			var _v0 = _Utils_Tuple2(path, prefix);
			if (!_v0.a.b) {
				return true;
			} else {
				if (!_v0.b.b) {
					return false;
				} else {
					var _v1 = _v0.a;
					var pathHead = _v1.a;
					var pathTail = _v1.b;
					var _v2 = _v0.b;
					var prefixHead = _v2.a;
					var prefixTail = _v2.b;
					if (_Utils_eq(prefixHead, pathHead)) {
						var $temp$prefix = prefixTail,
							$temp$path = pathTail;
						prefix = $temp$prefix;
						path = $temp$path;
						continue isPrefixOf;
					} else {
						return false;
					}
				}
			}
		}
	});
var $author$project$Morphir$Elm$Frontend$Resolve$moduleMapping = function () {
	var sdkModule = function (m) {
		return A2(
			$elm$core$List$append,
			$author$project$Morphir$IR$Path$fromString('Morphir.SDK'),
			_List_fromArray(
				[m]));
	};
	return $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['basics'])
					]),
				sdkModule(
					_List_fromArray(
						['basics']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['list'])
					]),
				sdkModule(
					_List_fromArray(
						['list']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['dict'])
					]),
				sdkModule(
					_List_fromArray(
						['dict']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['set'])
					]),
				sdkModule(
					_List_fromArray(
						['set']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['maybe'])
					]),
				sdkModule(
					_List_fromArray(
						['maybe']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['result'])
					]),
				sdkModule(
					_List_fromArray(
						['result']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['string'])
					]),
				sdkModule(
					_List_fromArray(
						['string']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['char'])
					]),
				sdkModule(
					_List_fromArray(
						['char']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['tuple'])
					]),
				sdkModule(
					_List_fromArray(
						['tuple']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['regex'])
					]),
				sdkModule(
					_List_fromArray(
						['regex']))),
				_Utils_Tuple2(
				_List_fromArray(
					[
						_List_fromArray(
						['decimal'])
					]),
				sdkModule(
					_List_fromArray(
						['decimal'])))
			]));
}();
var $author$project$Morphir$Elm$Frontend$Resolve$locateModule = F2(
	function (packageSpecs, packageAndModulePath) {
		var mappedPackageAndModulePath = A2(
			$elm$core$Maybe$withDefault,
			packageAndModulePath,
			A2($elm$core$Dict$get, packageAndModulePath, $author$project$Morphir$Elm$Frontend$Resolve$moduleMapping));
		var matchingModules = A2(
			$elm$core$List$filterMap,
			function (_v2) {
				var packagePath = _v2.a;
				var packageSpec = _v2.b;
				if (A2($author$project$Morphir$IR$Path$isPrefixOf, mappedPackageAndModulePath, packagePath)) {
					var modulePath = A2(
						$elm$core$List$drop,
						$elm$core$List$length(packagePath),
						mappedPackageAndModulePath);
					return A2(
						$elm$core$Maybe$map,
						function (moduleSpec) {
							return _Utils_Tuple3(packagePath, modulePath, moduleSpec);
						},
						A2($elm$core$Dict$get, modulePath, packageSpec.e7));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			$elm$core$Dict$toList(packageSpecs));
		if (!matchingModules.b) {
			return $elm$core$Result$Err(
				$author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindModule(packageAndModulePath));
		} else {
			if (!matchingModules.b.b) {
				var matchingModule = matchingModules.a;
				return $elm$core$Result$Ok(matchingModule);
			} else {
				var multipleMatches = matchingModules;
				return $elm$core$Result$Err(
					A2(
						$author$project$Morphir$Elm$Frontend$Resolve$AmbiguousModulePath,
						packageAndModulePath,
						A2(
							$elm$core$List$map,
							function (_v1) {
								var packagePath = _v1.a;
								var modulePath = _v1.b;
								return _Utils_Tuple2(packagePath, modulePath);
							},
							multipleMatches)));
			}
		}
	});
var $author$project$Morphir$Elm$Frontend$Resolve$moduleSpecToLocalNames = function (moduleSpec) {
	return {
		H: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$filterMap,
				function (_v0) {
					var typeName = _v0.a;
					var typeSpec = _v0.b;
					var _v1 = typeSpec.aT;
					switch (_v1.$) {
						case 1:
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(typeName, _List_Nil));
						case 2:
							var ctors = _v1.b;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									typeName,
									A2(
										$elm$core$List$map,
										function (_v2) {
											var ctorName = _v2.a;
											return ctorName;
										},
										ctors)));
						default:
							if (_v1.b.$ === 3) {
								var _v3 = _v1.b;
								return $elm$core$Maybe$Just(
									_Utils_Tuple2(
										typeName,
										_List_fromArray(
											[typeName])));
							} else {
								return $elm$core$Maybe$Nothing;
							}
					}
				},
				$elm$core$Dict$toList(moduleSpec.gn))),
		W: $elm$core$Dict$keys(moduleSpec.gn),
		X: $elm$core$Dict$keys(moduleSpec.gq)
	};
};
var $author$project$Morphir$Elm$Frontend$Resolve$createModuleResolver = function (ctx) {
	var packageSpecs = function () {
		var currentPackageSpec = $author$project$Morphir$IR$Package$Specification(ctx.ce);
		return A3($elm$core$Dict$insert, ctx.bg, currentPackageSpec, ctx.ch);
	}();
	var localNames = {
		H: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$filterMap,
				function (_v7) {
					var typeName = _v7.a;
					var accessControlledDocumentedTypeDef = _v7.b;
					var _v8 = accessControlledDocumentedTypeDef.aT.aT;
					if (_v8.$ === 1) {
						var accessControlledCtors = _v8.b;
						return $elm$core$Maybe$Just(
							_Utils_Tuple2(
								typeName,
								A2(
									$elm$core$List$map,
									function (_v9) {
										var name = _v9.a;
										return name;
									},
									accessControlledCtors.aT)));
					} else {
						if (_v8.b.$ === 3) {
							var _v10 = _v8.b;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									typeName,
									_List_fromArray(
										[typeName])));
						} else {
							return $elm$core$Maybe$Nothing;
						}
					}
				},
				$elm$core$Dict$toList(ctx.a5.gn))),
		W: $elm$core$Dict$keys(ctx.a5.gn),
		X: $elm$core$Dict$keys(ctx.a5.gq)
	};
	var imports = _Utils_ap($author$project$Morphir$Elm$Frontend$Resolve$defaultImports, ctx.cm);
	var importedNamesResult = A2(
		$author$project$Morphir$Elm$Frontend$Resolve$collectImportedNames,
		function (packageAndModulePath) {
			return A2(
				$elm$core$Result$map,
				function (_v6) {
					var packagePath = _v6.a;
					var modulePath = _v6.b;
					var moduleSpec = _v6.c;
					return _Utils_Tuple3(
						packagePath,
						modulePath,
						$author$project$Morphir$Elm$Frontend$Resolve$moduleSpecToLocalNames(moduleSpec));
				},
				A2($author$project$Morphir$Elm$Frontend$Resolve$locateModule, packageSpecs, packageAndModulePath));
		},
		imports);
	var resolveWithoutModuleName = F3(
		function (trace, nameType, sourceLocalName) {
			var localName = $author$project$Morphir$IR$Name$fromString(sourceLocalName);
			var localToFullyQualified = function (imported) {
				return A2(
					$elm$core$Result$andThen,
					function (modulePaths) {
						if (!modulePaths.b) {
							return $elm$core$Result$Err(
								A3($author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindLocalName, trace, nameType, sourceLocalName));
						} else {
							if (!modulePaths.b.b) {
								var _v5 = modulePaths.a;
								var packagePath = _v5.a;
								var modulePath = _v5.b;
								return $elm$core$Result$Ok(
									A3($author$project$Morphir$IR$FQName$fQName, packagePath, modulePath, localName));
							} else {
								return $elm$core$Result$Err(
									A2($author$project$Morphir$Elm$Frontend$Resolve$AmbiguousImports, sourceLocalName, modulePaths));
							}
						}
					},
					A2(
						$elm$core$Result$fromMaybe,
						A3($author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindLocalName, trace, nameType, sourceLocalName),
						A2($elm$core$Dict$get, localName, imported)));
			};
			return A2(
				$elm$core$Result$andThen,
				function (importedNames) {
					switch (nameType) {
						case 0:
							return localToFullyQualified(importedNames.W);
						case 1:
							return localToFullyQualified(importedNames.H);
						default:
							return localToFullyQualified(importedNames.X);
					}
				},
				importedNamesResult);
		});
	var importedModulesResult = A2(
		$elm$core$Result$map,
		A2($elm$core$Basics$composeR, $elm$core$List$concat, $elm$core$Dict$fromList),
		$author$project$Morphir$ListOfResults$liftFirstError(
			A2(
				$elm$core$List$map,
				function (imp) {
					var moduleName = $stil4m$elm_syntax$Elm$Syntax$Node$value(imp.bs);
					var _v1 = imp.e5;
					if (_v1.$ === 1) {
						return A2(
							$elm$core$Result$map,
							function (resolved) {
								return _List_fromArray(
									[
										_Utils_Tuple2(moduleName, resolved)
									]);
							},
							A2(
								$author$project$Morphir$Elm$Frontend$Resolve$locateModule,
								packageSpecs,
								A2($elm$core$List$map, $author$project$Morphir$IR$Name$fromString, moduleName)));
					} else {
						var _v2 = _v1.a;
						var alias = _v2.b;
						return A2(
							$elm$core$Result$map,
							function (resolved) {
								return _List_fromArray(
									[
										_Utils_Tuple2(moduleName, resolved),
										_Utils_Tuple2(alias, resolved)
									]);
							},
							A2(
								$author$project$Morphir$Elm$Frontend$Resolve$locateModule,
								packageSpecs,
								A2($elm$core$List$map, $author$project$Morphir$IR$Name$fromString, moduleName)));
					}
				},
				imports)));
	var resolveWithModuleName = F4(
		function (trace, nameType, sourceModuleName, sourceLocalName) {
			var localName = $author$project$Morphir$IR$Name$fromString(sourceLocalName);
			return A2(
				$elm$core$Result$andThen,
				function (importedModules) {
					return A2(
						$elm$core$Result$andThen,
						function (_v0) {
							var packagePath = _v0.a;
							var modulePath = _v0.b;
							var moduleSpec = _v0.c;
							return A3(
								$author$project$Morphir$Elm$Frontend$Resolve$isAmongLocalNames,
								nameType,
								localName,
								$author$project$Morphir$Elm$Frontend$Resolve$moduleSpecToLocalNames(moduleSpec)) ? $elm$core$Result$Ok(
								A3($author$project$Morphir$IR$FQName$fQName, packagePath, modulePath, localName)) : $elm$core$Result$Err(
								A4($author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindNameInModule, nameType, packagePath, modulePath, localName));
						},
						A2(
							$elm$core$Result$fromMaybe,
							$author$project$Morphir$Elm$Frontend$Resolve$CouldNotFindModule(
								A2($elm$core$List$map, $author$project$Morphir$IR$Name$fromString, sourceModuleName)),
							A2($elm$core$Dict$get, sourceModuleName, importedModules)));
				},
				importedModulesResult);
		});
	var resolve = F3(
		function (nameType, elmModuleName, elmLocalNameToResolve) {
			var trace = A3($author$project$Morphir$Elm$Frontend$Resolve$ResolveTarget, nameType, elmModuleName, elmLocalNameToResolve);
			var localNameToResolve = $author$project$Morphir$IR$Name$fromString(elmLocalNameToResolve);
			return $elm$core$List$isEmpty(elmModuleName) ? (A3($author$project$Morphir$Elm$Frontend$Resolve$isAmongLocalNames, nameType, localNameToResolve, localNames) ? $elm$core$Result$Ok(
				A3($author$project$Morphir$IR$FQName$fQName, ctx.bg, ctx.cd, localNameToResolve)) : A3(
				resolveWithoutModuleName,
				A2($author$project$Morphir$Elm$Frontend$Resolve$ScannedLocalNames, localNames, trace),
				nameType,
				elmLocalNameToResolve)) : A4(resolveWithModuleName, trace, nameType, elmModuleName, elmLocalNameToResolve);
		});
	return A3(
		$author$project$Morphir$Elm$Frontend$Resolve$ModuleResolver,
		resolve(0),
		resolve(1),
		resolve(2));
};
var $author$project$Morphir$IR$Type$mapSpecificationAttributes = F2(
	function (f, spec) {
		switch (spec.$) {
			case 0:
				var params = spec.a;
				var tpe = spec.b;
				return A2(
					$author$project$Morphir$IR$Type$TypeAliasSpecification,
					params,
					A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, tpe));
			case 1:
				var params = spec.a;
				return $author$project$Morphir$IR$Type$OpaqueTypeSpecification(params);
			default:
				var params = spec.a;
				var constructors = spec.b;
				return A2(
					$author$project$Morphir$IR$Type$CustomTypeSpecification,
					params,
					A2(
						$elm$core$List$map,
						function (_v1) {
							var ctorName = _v1.a;
							var ctorArgs = _v1.b;
							return A2(
								$author$project$Morphir$IR$Type$Constructor,
								ctorName,
								A2(
									$elm$core$List$map,
									function (_v2) {
										var argName = _v2.a;
										var argType = _v2.b;
										return _Utils_Tuple2(
											argName,
											A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, argType));
									},
									ctorArgs));
						},
						constructors));
		}
	});
var $author$project$Morphir$IR$Value$mapSpecificationAttributes = F2(
	function (f, spec) {
		return A2(
			$author$project$Morphir$IR$Value$Specification,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var tpe = _v0.b;
					return _Utils_Tuple2(
						name,
						A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, tpe));
				},
				spec.bo),
			A2($author$project$Morphir$IR$Type$mapTypeAttributes, f, spec.bN));
	});
var $author$project$Morphir$IR$Module$mapSpecificationAttributes = F2(
	function (tf, spec) {
		return A2(
			$author$project$Morphir$IR$Module$Specification,
			A2(
				$elm$core$Dict$map,
				F2(
					function (_v0, typeSpec) {
						return A2(
							$author$project$Morphir$IR$Documented$map,
							$author$project$Morphir$IR$Type$mapSpecificationAttributes(tf),
							typeSpec);
					}),
				spec.gn),
			A2(
				$elm$core$Dict$map,
				F2(
					function (_v1, valueSpec) {
						return A2($author$project$Morphir$IR$Value$mapSpecificationAttributes, tf, valueSpec);
					}),
				spec.gq));
	});
var $author$project$Morphir$IR$Module$eraseSpecificationAttributes = function (spec) {
	return A2(
		$author$project$Morphir$IR$Module$mapSpecificationAttributes,
		function (_v0) {
			return 0;
		},
		spec);
};
var $author$project$Morphir$IR$Type$customTypeDefinition = F2(
	function (typeParams, ctors) {
		return A2($author$project$Morphir$IR$Type$CustomTypeDefinition, typeParams, ctors);
	});
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $author$project$Morphir$Elm$Frontend$SourceLocation = F2(
	function (source, range) {
		return {c$: range, de: source};
	});
var $author$project$Morphir$Elm$Frontend$mapTypeAnnotation = F2(
	function (sourceFile, _v0) {
		var range = _v0.a;
		var typeAnnotation = _v0.b;
		var sourceLocation = A2($author$project$Morphir$Elm$Frontend$SourceLocation, sourceFile, range);
		switch (typeAnnotation.$) {
			case 0:
				var varName = typeAnnotation.a;
				return $elm$core$Result$Ok(
					A2(
						$author$project$Morphir$IR$Type$Variable,
						sourceLocation,
						$author$project$Morphir$IR$Name$fromString(varName)));
			case 1:
				var _v2 = typeAnnotation.a;
				var _v3 = _v2.b;
				var moduleName = _v3.a;
				var localName = _v3.b;
				var argNodes = typeAnnotation.b;
				return A2(
					$elm$core$Result$map,
					A2(
						$author$project$Morphir$IR$Type$Reference,
						sourceLocation,
						A3(
							$author$project$Morphir$IR$FQName$fQName,
							_List_Nil,
							A2($elm$core$List$map, $author$project$Morphir$IR$Name$fromString, moduleName),
							$author$project$Morphir$IR$Name$fromString(localName))),
					A2(
						$elm$core$Result$mapError,
						$elm$core$List$concat,
						$author$project$Morphir$ListOfResults$liftAllErrors(
							A2(
								$elm$core$List$map,
								$author$project$Morphir$Elm$Frontend$mapTypeAnnotation(sourceFile),
								argNodes))));
			case 2:
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$Type$Unit(sourceLocation));
			case 3:
				var elemNodes = typeAnnotation.a;
				return A2(
					$elm$core$Result$mapError,
					$elm$core$List$concat,
					A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Type$Tuple(sourceLocation),
						$author$project$Morphir$ListOfResults$liftAllErrors(
							A2(
								$elm$core$List$map,
								$author$project$Morphir$Elm$Frontend$mapTypeAnnotation(sourceFile),
								elemNodes))));
			case 4:
				var fieldNodes = typeAnnotation.a;
				return A2(
					$elm$core$Result$mapError,
					$elm$core$List$concat,
					A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Type$Record(sourceLocation),
						$author$project$Morphir$ListOfResults$liftAllErrors(
							A2(
								$elm$core$List$map,
								function (_v4) {
									var _v5 = _v4.a;
									var fieldName = _v5.b;
									var fieldTypeNode = _v4.b;
									return A2(
										$elm$core$Result$map,
										$author$project$Morphir$IR$Type$Field(
											$author$project$Morphir$IR$Name$fromString(fieldName)),
										A2($author$project$Morphir$Elm$Frontend$mapTypeAnnotation, sourceFile, fieldTypeNode));
								},
								A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, fieldNodes)))));
			case 5:
				var _v6 = typeAnnotation.a;
				var argName = _v6.b;
				var _v7 = typeAnnotation.b;
				var fieldNodes = _v7.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$core$List$concat,
					A2(
						$elm$core$Result$map,
						A2(
							$author$project$Morphir$IR$Type$ExtensibleRecord,
							sourceLocation,
							$author$project$Morphir$IR$Name$fromString(argName)),
						$author$project$Morphir$ListOfResults$liftAllErrors(
							A2(
								$elm$core$List$map,
								function (_v8) {
									var _v9 = _v8.a;
									var fieldName = _v9.b;
									var fieldTypeNode = _v8.b;
									return A2(
										$elm$core$Result$map,
										$author$project$Morphir$IR$Type$Field(
											$author$project$Morphir$IR$Name$fromString(fieldName)),
										A2($author$project$Morphir$Elm$Frontend$mapTypeAnnotation, sourceFile, fieldTypeNode));
								},
								A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, fieldNodes)))));
			default:
				var argTypeNode = typeAnnotation.a;
				var returnTypeNode = typeAnnotation.b;
				return A3(
					$elm$core$Result$map2,
					$author$project$Morphir$IR$Type$Function(sourceLocation),
					A2($author$project$Morphir$Elm$Frontend$mapTypeAnnotation, sourceFile, argTypeNode),
					A2($author$project$Morphir$Elm$Frontend$mapTypeAnnotation, sourceFile, returnTypeNode));
		}
	});
var $author$project$Morphir$IR$Type$typeAliasDefinition = F2(
	function (typeParams, typeExp) {
		return A2($author$project$Morphir$IR$Type$TypeAliasDefinition, typeParams, typeExp);
	});
var $author$project$Morphir$IR$AccessControlled$private = function (value) {
	return A2($author$project$Morphir$IR$AccessControlled$AccessControlled, 1, value);
};
var $author$project$Morphir$IR$AccessControlled$public = function (value) {
	return A2($author$project$Morphir$IR$AccessControlled$AccessControlled, 0, value);
};
var $author$project$Morphir$Elm$Frontend$withAccessControl = F2(
	function (isExposed, a) {
		return isExposed ? $author$project$Morphir$IR$AccessControlled$public(a) : $author$project$Morphir$IR$AccessControlled$private(a);
	});
var $author$project$Morphir$Elm$Frontend$mapDeclarationsToType = F3(
	function (sourceFile, expose, decls) {
		return A2(
			$elm$core$Result$mapError,
			$elm$core$List$concat,
			$author$project$Morphir$ListOfResults$liftAllErrors(
				A2(
					$elm$core$List$filterMap,
					function (decl) {
						switch (decl.$) {
							case 1:
								var typeAlias = decl.a;
								return $elm$core$Maybe$Just(
									A2(
										$elm$core$Result$map,
										function (typeExp) {
											var typeParams = A2(
												$elm$core$List$map,
												A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, $author$project$Morphir$IR$Name$fromString),
												typeAlias.cr);
											var name = $author$project$Morphir$IR$Name$fromString(
												$stil4m$elm_syntax$Elm$Syntax$Node$value(typeAlias.cN));
											var isExposed = function () {
												if (!expose.$) {
													return true;
												} else {
													var exposeList = expose.a;
													return A2(
														$elm$core$List$any,
														function (topLevelExpose) {
															if (topLevelExpose.$ === 2) {
																var exposedName = topLevelExpose.a;
																return _Utils_eq(
																	exposedName,
																	$stil4m$elm_syntax$Elm$Syntax$Node$value(typeAlias.cN));
															} else {
																return false;
															}
														},
														A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, exposeList));
												}
											}();
											var doc = A2(
												$elm$core$Maybe$withDefault,
												'',
												A2(
													$elm$core$Maybe$map,
													A2(
														$elm$core$Basics$composeR,
														$stil4m$elm_syntax$Elm$Syntax$Node$value,
														A2(
															$elm$core$Basics$composeR,
															$elm$core$String$dropLeft(3),
															$elm$core$String$dropRight(2))),
													typeAlias.ci));
											return _Utils_Tuple2(
												name,
												A2(
													$author$project$Morphir$Elm$Frontend$withAccessControl,
													isExposed,
													A2(
														$author$project$Morphir$IR$Documented$Documented,
														doc,
														A2($author$project$Morphir$IR$Type$typeAliasDefinition, typeParams, typeExp))));
										},
										A2($author$project$Morphir$Elm$Frontend$mapTypeAnnotation, sourceFile, typeAlias.dn)));
							case 2:
								var customType = decl.a;
								var typeParams = A2(
									$elm$core$List$map,
									A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, $author$project$Morphir$IR$Name$fromString),
									customType.cr);
								var name = $author$project$Morphir$IR$Name$fromString(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(customType.cN));
								var doc = A2(
									$elm$core$Maybe$withDefault,
									'',
									A2(
										$elm$core$Maybe$map,
										A2(
											$elm$core$Basics$composeR,
											$stil4m$elm_syntax$Elm$Syntax$Node$value,
											A2(
												$elm$core$Basics$composeR,
												$elm$core$String$dropLeft(3),
												$elm$core$String$dropRight(2))),
										customType.ci));
								var ctorsResult = A2(
									$elm$core$Result$mapError,
									$elm$core$List$concat,
									$author$project$Morphir$ListOfResults$liftAllErrors(
										A2(
											$elm$core$List$map,
											function (ctorNode) {
												var ctor = $stil4m$elm_syntax$Elm$Syntax$Node$value(ctorNode);
												var ctorArgsResult = A2(
													$elm$core$Result$mapError,
													$elm$core$List$concat,
													$author$project$Morphir$ListOfResults$liftAllErrors(
														A2(
															$elm$core$List$indexedMap,
															F2(
																function (index, arg) {
																	return A2(
																		$elm$core$Result$map,
																		function (argType) {
																			return _Utils_Tuple2(
																				_List_fromArray(
																					[
																						'arg',
																						$elm$core$String$fromInt(index + 1)
																					]),
																				argType);
																		},
																		A2($author$project$Morphir$Elm$Frontend$mapTypeAnnotation, sourceFile, arg));
																}),
															ctor.b4)));
												var ctorName = $author$project$Morphir$IR$Name$fromString(
													$stil4m$elm_syntax$Elm$Syntax$Node$value(ctor.cN));
												return A2(
													$elm$core$Result$map,
													function (ctorArgs) {
														return A2($author$project$Morphir$IR$Type$Constructor, ctorName, ctorArgs);
													},
													ctorArgsResult);
											},
											customType.ed)));
								var _v3 = function () {
									if (!expose.$) {
										return _Utils_Tuple2(true, true);
									} else {
										var exposeList = expose.a;
										return A2(
											$elm$core$Maybe$withDefault,
											_Utils_Tuple2(false, false),
											A2(
												$elm$core$Maybe$map,
												function (isOpen) {
													return _Utils_Tuple2(true, isOpen);
												},
												$elm$core$List$head(
													A2(
														$elm$core$List$filterMap,
														function (topLevelExpose) {
															switch (topLevelExpose.$) {
																case 2:
																	var exposedName = topLevelExpose.a;
																	return _Utils_eq(
																		exposedName,
																		$stil4m$elm_syntax$Elm$Syntax$Node$value(customType.cN)) ? $elm$core$Maybe$Just(false) : $elm$core$Maybe$Nothing;
																case 3:
																	var exposedType = topLevelExpose.a;
																	if (_Utils_eq(
																		exposedType.cN,
																		$stil4m$elm_syntax$Elm$Syntax$Node$value(customType.cN))) {
																		var _v6 = exposedType.fo;
																		if (!_v6.$) {
																			return $elm$core$Maybe$Just(true);
																		} else {
																			return $elm$core$Maybe$Just(false);
																		}
																	} else {
																		return $elm$core$Maybe$Nothing;
																	}
																default:
																	return $elm$core$Maybe$Nothing;
															}
														},
														A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, exposeList)))));
									}
								}();
								var isTypeExposed = _v3.a;
								var isCtorExposed = _v3.b;
								return $elm$core$Maybe$Just(
									A2(
										$elm$core$Result$map,
										function (constructors) {
											return _Utils_Tuple2(
												name,
												A2(
													$author$project$Morphir$Elm$Frontend$withAccessControl,
													isTypeExposed,
													A2(
														$author$project$Morphir$IR$Documented$Documented,
														doc,
														A2(
															$author$project$Morphir$IR$Type$customTypeDefinition,
															typeParams,
															A2($author$project$Morphir$Elm$Frontend$withAccessControl, isCtorExposed, constructors)))));
										},
										ctorsResult));
							default:
								return $elm$core$Maybe$Nothing;
						}
					},
					decls)));
	});
var $elm_community$graph$Graph$Edge = F3(
	function (from, to, label) {
		return {aA: from, bp: label, aD: to};
	});
var $author$project$Morphir$Elm$Frontend$EmptyApply = function (a) {
	return {$: 3, a: a};
};
var $author$project$Morphir$Elm$Frontend$MissingTypeSignature = function (a) {
	return {$: 7, a: a};
};
var $elm_community$graph$Graph$Node = F2(
	function (id, label) {
		return {L: id, bp: label};
	});
var $author$project$Morphir$Elm$Frontend$NotSupported = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm_community$graph$Graph$AcyclicGraph = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm_community$intdict$IntDict$Empty = {$: 0};
var $elm_community$intdict$IntDict$empty = $elm_community$intdict$IntDict$Empty;
var $elm_community$intdict$IntDict$findMin = function (dict) {
	findMin:
	while (true) {
		switch (dict.$) {
			case 0:
				return $elm$core$Maybe$Nothing;
			case 1:
				var l = dict.a;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(l.cG, l.aT));
			default:
				var i = dict.a;
				var $temp$dict = i.c;
				dict = $temp$dict;
				continue findMin;
		}
	}
};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm_community$intdict$IntDict$Inner = function (a) {
	return {$: 2, a: a};
};
var $elm_community$intdict$IntDict$size = function (dict) {
	switch (dict.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		default:
			var i = dict.a;
			return i.dc;
	}
};
var $elm_community$intdict$IntDict$inner = F3(
	function (p, l, r) {
		var _v0 = _Utils_Tuple2(l, r);
		if (!_v0.a.$) {
			var _v1 = _v0.a;
			return r;
		} else {
			if (!_v0.b.$) {
				var _v2 = _v0.b;
				return l;
			} else {
				return $elm_community$intdict$IntDict$Inner(
					{
						c: l,
						g: p,
						d: r,
						dc: $elm_community$intdict$IntDict$size(l) + $elm_community$intdict$IntDict$size(r)
					});
			}
		}
	});
var $elm$core$Bitwise$complement = _Bitwise_complement;
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm_community$intdict$IntDict$highestBitSet = function (n) {
	var shiftOr = F2(
		function (i, shift) {
			return i | (i >>> shift);
		});
	var n1 = A2(shiftOr, n, 1);
	var n2 = A2(shiftOr, n1, 2);
	var n3 = A2(shiftOr, n2, 4);
	var n4 = A2(shiftOr, n3, 8);
	var n5 = A2(shiftOr, n4, 16);
	return n5 & (~(n5 >>> 1));
};
var $elm_community$intdict$IntDict$signBit = $elm_community$intdict$IntDict$highestBitSet(-1);
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm_community$intdict$IntDict$isBranchingBitSet = function (p) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$Bitwise$xor($elm_community$intdict$IntDict$signBit),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Bitwise$and(p.aw),
			$elm$core$Basics$neq(0)));
};
var $elm_community$intdict$IntDict$higherBitMask = function (branchingBit) {
	return branchingBit ^ (~(branchingBit - 1));
};
var $elm_community$intdict$IntDict$lcp = F2(
	function (x, y) {
		var branchingBit = $elm_community$intdict$IntDict$highestBitSet(x ^ y);
		var mask = $elm_community$intdict$IntDict$higherBitMask(branchingBit);
		var prefixBits = x & mask;
		return {aw: branchingBit, T: prefixBits};
	});
var $elm_community$intdict$IntDict$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm_community$intdict$IntDict$leaf = F2(
	function (k, v) {
		return $elm_community$intdict$IntDict$Leaf(
			{cG: k, aT: v});
	});
var $elm_community$intdict$IntDict$prefixMatches = F2(
	function (p, n) {
		return _Utils_eq(
			n & $elm_community$intdict$IntDict$higherBitMask(p.aw),
			p.T);
	});
var $elm_community$intdict$IntDict$update = F3(
	function (key, alter, dict) {
		var join = F2(
			function (_v2, _v3) {
				var k1 = _v2.a;
				var l = _v2.b;
				var k2 = _v3.a;
				var r = _v3.b;
				var prefix = A2($elm_community$intdict$IntDict$lcp, k1, k2);
				return A2($elm_community$intdict$IntDict$isBranchingBitSet, prefix, k2) ? A3($elm_community$intdict$IntDict$inner, prefix, l, r) : A3($elm_community$intdict$IntDict$inner, prefix, r, l);
			});
		var alteredNode = function (mv) {
			var _v1 = alter(mv);
			if (!_v1.$) {
				var v = _v1.a;
				return A2($elm_community$intdict$IntDict$leaf, key, v);
			} else {
				return $elm_community$intdict$IntDict$empty;
			}
		};
		switch (dict.$) {
			case 0:
				return alteredNode($elm$core$Maybe$Nothing);
			case 1:
				var l = dict.a;
				return _Utils_eq(l.cG, key) ? alteredNode(
					$elm$core$Maybe$Just(l.aT)) : A2(
					join,
					_Utils_Tuple2(
						key,
						alteredNode($elm$core$Maybe$Nothing)),
					_Utils_Tuple2(l.cG, dict));
			default:
				var i = dict.a;
				return A2($elm_community$intdict$IntDict$prefixMatches, i.g, key) ? (A2($elm_community$intdict$IntDict$isBranchingBitSet, i.g, key) ? A3(
					$elm_community$intdict$IntDict$inner,
					i.g,
					i.c,
					A3($elm_community$intdict$IntDict$update, key, alter, i.d)) : A3(
					$elm_community$intdict$IntDict$inner,
					i.g,
					A3($elm_community$intdict$IntDict$update, key, alter, i.c),
					i.d)) : A2(
					join,
					_Utils_Tuple2(
						key,
						alteredNode($elm$core$Maybe$Nothing)),
					_Utils_Tuple2(i.g.T, dict));
		}
	});
var $elm_community$intdict$IntDict$insert = F3(
	function (key, value, dict) {
		return A3(
			$elm_community$intdict$IntDict$update,
			key,
			$elm$core$Basics$always(
				$elm$core$Maybe$Just(value)),
			dict);
	});
var $elm_community$intdict$IntDict$Disjunct = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm_community$intdict$IntDict$Left = 0;
var $elm_community$intdict$IntDict$Parent = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm_community$intdict$IntDict$Right = 1;
var $elm_community$intdict$IntDict$SamePrefix = {$: 0};
var $elm_community$intdict$IntDict$combineBits = F3(
	function (a, b, mask) {
		return (a & (~mask)) | (b & mask);
	});
var $elm_community$intdict$IntDict$mostSignificantBranchingBit = F2(
	function (a, b) {
		return (_Utils_eq(a, $elm_community$intdict$IntDict$signBit) || _Utils_eq(b, $elm_community$intdict$IntDict$signBit)) ? $elm_community$intdict$IntDict$signBit : A2($elm$core$Basics$max, a, b);
	});
var $elm_community$intdict$IntDict$determineBranchRelation = F2(
	function (l, r) {
		var rp = r.g;
		var lp = l.g;
		var mask = $elm_community$intdict$IntDict$highestBitSet(
			A2($elm_community$intdict$IntDict$mostSignificantBranchingBit, lp.aw, rp.aw));
		var modifiedRightPrefix = A3($elm_community$intdict$IntDict$combineBits, rp.T, ~lp.T, mask);
		var prefix = A2($elm_community$intdict$IntDict$lcp, lp.T, modifiedRightPrefix);
		var childEdge = F2(
			function (branchPrefix, c) {
				return A2($elm_community$intdict$IntDict$isBranchingBitSet, branchPrefix, c.g.T) ? 1 : 0;
			});
		return _Utils_eq(lp, rp) ? $elm_community$intdict$IntDict$SamePrefix : (_Utils_eq(prefix, lp) ? A2(
			$elm_community$intdict$IntDict$Parent,
			0,
			A2(childEdge, l.g, r)) : (_Utils_eq(prefix, rp) ? A2(
			$elm_community$intdict$IntDict$Parent,
			1,
			A2(childEdge, r.g, l)) : A2(
			$elm_community$intdict$IntDict$Disjunct,
			prefix,
			A2(childEdge, prefix, l))));
	});
var $elm_community$intdict$IntDict$get = F2(
	function (key, dict) {
		get:
		while (true) {
			switch (dict.$) {
				case 0:
					return $elm$core$Maybe$Nothing;
				case 1:
					var l = dict.a;
					return _Utils_eq(l.cG, key) ? $elm$core$Maybe$Just(l.aT) : $elm$core$Maybe$Nothing;
				default:
					var i = dict.a;
					if (!A2($elm_community$intdict$IntDict$prefixMatches, i.g, key)) {
						return $elm$core$Maybe$Nothing;
					} else {
						if (A2($elm_community$intdict$IntDict$isBranchingBitSet, i.g, key)) {
							var $temp$key = key,
								$temp$dict = i.d;
							key = $temp$key;
							dict = $temp$dict;
							continue get;
						} else {
							var $temp$key = key,
								$temp$dict = i.c;
							key = $temp$key;
							dict = $temp$dict;
							continue get;
						}
					}
			}
		}
	});
var $elm_community$intdict$IntDict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm_community$intdict$IntDict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm_community$intdict$IntDict$intersect = F2(
	function (l, r) {
		intersect:
		while (true) {
			var _v0 = _Utils_Tuple2(l, r);
			_v0$1:
			while (true) {
				_v0$2:
				while (true) {
					switch (_v0.a.$) {
						case 0:
							var _v1 = _v0.a;
							return $elm_community$intdict$IntDict$Empty;
						case 1:
							switch (_v0.b.$) {
								case 0:
									break _v0$1;
								case 1:
									break _v0$2;
								default:
									break _v0$2;
							}
						default:
							switch (_v0.b.$) {
								case 0:
									break _v0$1;
								case 1:
									var lr = _v0.b.a;
									var _v3 = A2($elm_community$intdict$IntDict$get, lr.cG, l);
									if (!_v3.$) {
										var v = _v3.a;
										return A2($elm_community$intdict$IntDict$leaf, lr.cG, v);
									} else {
										return $elm_community$intdict$IntDict$Empty;
									}
								default:
									var il = _v0.a.a;
									var ir = _v0.b.a;
									var _v4 = A2($elm_community$intdict$IntDict$determineBranchRelation, il, ir);
									switch (_v4.$) {
										case 0:
											return A3(
												$elm_community$intdict$IntDict$inner,
												il.g,
												A2($elm_community$intdict$IntDict$intersect, il.c, ir.c),
												A2($elm_community$intdict$IntDict$intersect, il.d, ir.d));
										case 1:
											if (!_v4.a) {
												if (_v4.b === 1) {
													var _v5 = _v4.a;
													var _v6 = _v4.b;
													var $temp$l = il.d,
														$temp$r = r;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												} else {
													var _v9 = _v4.a;
													var _v10 = _v4.b;
													var $temp$l = il.c,
														$temp$r = r;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												}
											} else {
												if (_v4.b === 1) {
													var _v7 = _v4.a;
													var _v8 = _v4.b;
													var $temp$l = l,
														$temp$r = ir.d;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												} else {
													var _v11 = _v4.a;
													var _v12 = _v4.b;
													var $temp$l = l,
														$temp$r = ir.c;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												}
											}
										default:
											return $elm_community$intdict$IntDict$Empty;
									}
							}
					}
				}
				var ll = _v0.a.a;
				return A2($elm_community$intdict$IntDict$member, ll.cG, r) ? l : $elm_community$intdict$IntDict$Empty;
			}
			var _v2 = _v0.b;
			return $elm_community$intdict$IntDict$Empty;
		}
	});
var $elm_community$graph$Graph$crashHack = function (msg) {
	crashHack:
	while (true) {
		var $temp$msg = msg;
		msg = $temp$msg;
		continue crashHack;
	}
};
var $elm_community$graph$Graph$unGraph = function (graph) {
	var rep = graph;
	return rep;
};
var $elm_community$graph$Graph$get = function (nodeId) {
	return A2(
		$elm$core$Basics$composeR,
		$elm_community$graph$Graph$unGraph,
		$elm_community$intdict$IntDict$get(nodeId));
};
var $elm_community$graph$Graph$unsafeGet = F3(
	function (msg, id, graph) {
		var _v0 = A2($elm_community$graph$Graph$get, id, graph);
		if (_v0.$ === 1) {
			return $elm_community$graph$Graph$crashHack(msg);
		} else {
			var ctx = _v0.a;
			return ctx;
		}
	});
var $elm_community$graph$Graph$checkForBackEdges = F2(
	function (ordering, graph) {
		var success = function (_v3) {
			return A2($elm_community$graph$Graph$AcyclicGraph, graph, ordering);
		};
		var check = F2(
			function (id, _v2) {
				var backSet = _v2.a;
				var error = 'Graph.checkForBackEdges: `ordering` didn\'t contain `id`';
				var ctx = A3($elm_community$graph$Graph$unsafeGet, error, id, graph);
				var backSetWithId = A3($elm_community$intdict$IntDict$insert, id, 0, backSet);
				var backEdges = A2($elm_community$intdict$IntDict$intersect, ctx.h, backSetWithId);
				var _v0 = $elm_community$intdict$IntDict$findMin(backEdges);
				if (_v0.$ === 1) {
					return $elm$core$Result$Ok(
						_Utils_Tuple2(backSetWithId, 0));
				} else {
					var _v1 = _v0.a;
					var to = _v1.a;
					var label = _v1.b;
					return $elm$core$Result$Err(
						A3($elm_community$graph$Graph$Edge, id, to, label));
				}
			});
		return A2(
			$elm$core$Result$map,
			success,
			A3(
				$elm$core$List$foldl,
				F2(
					function (id, res) {
						return A2(
							$elm$core$Result$andThen,
							check(id),
							res);
					}),
				$elm$core$Result$Ok(
					_Utils_Tuple2($elm_community$intdict$IntDict$empty, 0)),
				ordering));
	});
var $elm_community$intdict$IntDict$foldr = F3(
	function (f, acc, dict) {
		foldr:
		while (true) {
			switch (dict.$) {
				case 0:
					return acc;
				case 1:
					var l = dict.a;
					return A3(f, l.cG, l.aT, acc);
				default:
					var i = dict.a;
					var $temp$f = f,
						$temp$acc = A3($elm_community$intdict$IntDict$foldr, f, acc, i.d),
						$temp$dict = i.c;
					f = $temp$f;
					acc = $temp$acc;
					dict = $temp$dict;
					continue foldr;
			}
		}
	});
var $elm_community$intdict$IntDict$keys = function (dict) {
	return A3(
		$elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm_community$graph$Graph$alongOutgoingEdges = function (ctx) {
	return $elm_community$intdict$IntDict$keys(ctx.h);
};
var $elm_community$graph$Graph$Graph = $elm$core$Basics$identity;
var $elm_community$intdict$IntDict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			switch (dict.$) {
				case 0:
					return acc;
				case 1:
					var l = dict.a;
					return A3(f, l.cG, l.aT, acc);
				default:
					var i = dict.a;
					var $temp$f = f,
						$temp$acc = A3($elm_community$intdict$IntDict$foldl, f, acc, i.c),
						$temp$dict = i.d;
					f = $temp$f;
					acc = $temp$acc;
					dict = $temp$dict;
					continue foldl;
			}
		}
	});
var $elm_community$graph$Graph$applyEdgeDiff = F3(
	function (nodeId, diff, graphRep) {
		var updateOutgoingEdge = F2(
			function (upd, node) {
				return _Utils_update(
					node,
					{
						h: A3($elm_community$intdict$IntDict$update, nodeId, upd, node.h)
					});
			});
		var updateIncomingEdge = F2(
			function (upd, node) {
				return _Utils_update(
					node,
					{
						i: A3($elm_community$intdict$IntDict$update, nodeId, upd, node.i)
					});
			});
		var flippedFoldl = F3(
			function (f, dict, acc) {
				return A3($elm_community$intdict$IntDict$foldl, f, acc, dict);
			});
		var edgeUpdateToMaybe = function (edgeUpdate) {
			if (!edgeUpdate.$) {
				var lbl = edgeUpdate.a;
				return $elm$core$Maybe$Just(lbl);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		var updateAdjacency = F3(
			function (updateEdge, updatedId, edgeUpdate) {
				var updateLbl = updateEdge(
					$elm$core$Basics$always(
						edgeUpdateToMaybe(edgeUpdate)));
				return A2(
					$elm_community$intdict$IntDict$update,
					updatedId,
					$elm$core$Maybe$map(updateLbl));
			});
		return A3(
			flippedFoldl,
			updateAdjacency(updateOutgoingEdge),
			diff.h,
			A3(
				flippedFoldl,
				updateAdjacency(updateIncomingEdge),
				diff.i,
				graphRep));
	});
var $elm_community$graph$Graph$Insert = function (a) {
	return {$: 0, a: a};
};
var $elm_community$graph$Graph$Remove = function (a) {
	return {$: 1, a: a};
};
var $elm_community$graph$Graph$emptyDiff = {i: $elm_community$intdict$IntDict$empty, h: $elm_community$intdict$IntDict$empty};
var $elm_community$graph$Graph$computeEdgeDiff = F2(
	function (old, _new) {
		var collectUpdates = F3(
			function (edgeUpdate, updatedId, label) {
				var replaceUpdate = function (old_) {
					var _v5 = _Utils_Tuple2(
						old_,
						edgeUpdate(label));
					if (!_v5.a.$) {
						if (_v5.a.a.$ === 1) {
							if (!_v5.b.$) {
								var oldLbl = _v5.a.a.a;
								var newLbl = _v5.b.a;
								return _Utils_eq(oldLbl, newLbl) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
									$elm_community$graph$Graph$Insert(newLbl));
							} else {
								return $elm_community$graph$Graph$crashHack('Graph.computeEdgeDiff: Collected two removals for the same edge. This is an error in the implementation of Graph and you should file a bug report!');
							}
						} else {
							return $elm_community$graph$Graph$crashHack('Graph.computeEdgeDiff: Collected inserts before removals. This is an error in the implementation of Graph and you should file a bug report!');
						}
					} else {
						var _v6 = _v5.a;
						var eu = _v5.b;
						return $elm$core$Maybe$Just(eu);
					}
				};
				return A2($elm_community$intdict$IntDict$update, updatedId, replaceUpdate);
			});
		var collect = F3(
			function (edgeUpdate, adj, updates) {
				return A3(
					$elm_community$intdict$IntDict$foldl,
					collectUpdates(edgeUpdate),
					updates,
					adj);
			});
		var _v0 = _Utils_Tuple2(old, _new);
		if (_v0.a.$ === 1) {
			if (_v0.b.$ === 1) {
				var _v1 = _v0.a;
				var _v2 = _v0.b;
				return $elm_community$graph$Graph$emptyDiff;
			} else {
				var _v4 = _v0.a;
				var ins = _v0.b.a;
				return {
					i: A3(collect, $elm_community$graph$Graph$Insert, ins.h, $elm_community$intdict$IntDict$empty),
					h: A3(collect, $elm_community$graph$Graph$Insert, ins.i, $elm_community$intdict$IntDict$empty)
				};
			}
		} else {
			if (_v0.b.$ === 1) {
				var rem = _v0.a.a;
				var _v3 = _v0.b;
				return {
					i: A3(collect, $elm_community$graph$Graph$Remove, rem.h, $elm_community$intdict$IntDict$empty),
					h: A3(collect, $elm_community$graph$Graph$Remove, rem.i, $elm_community$intdict$IntDict$empty)
				};
			} else {
				var rem = _v0.a.a;
				var ins = _v0.b.a;
				return _Utils_eq(rem, ins) ? $elm_community$graph$Graph$emptyDiff : {
					i: A3(
						collect,
						$elm_community$graph$Graph$Insert,
						ins.h,
						A3(collect, $elm_community$graph$Graph$Remove, rem.h, $elm_community$intdict$IntDict$empty)),
					h: A3(
						collect,
						$elm_community$graph$Graph$Insert,
						ins.i,
						A3(collect, $elm_community$graph$Graph$Remove, rem.i, $elm_community$intdict$IntDict$empty))
				};
			}
		}
	});
var $elm_community$intdict$IntDict$filter = F2(
	function (predicate, dict) {
		var add = F3(
			function (k, v, d) {
				return A2(predicate, k, v) ? A3($elm_community$intdict$IntDict$insert, k, v, d) : d;
			});
		return A3($elm_community$intdict$IntDict$foldl, add, $elm_community$intdict$IntDict$empty, dict);
	});
var $elm_community$graph$Graph$update = F2(
	function (nodeId, updater) {
		var wrappedUpdater = function (rep) {
			var old = A2($elm_community$intdict$IntDict$get, nodeId, rep);
			var filterInvalidEdges = function (ctx) {
				return $elm_community$intdict$IntDict$filter(
					F2(
						function (id, _v0) {
							return _Utils_eq(id, ctx.cP.L) || A2($elm_community$intdict$IntDict$member, id, rep);
						}));
			};
			var cleanUpEdges = function (ctx) {
				return _Utils_update(
					ctx,
					{
						i: A2(filterInvalidEdges, ctx, ctx.i),
						h: A2(filterInvalidEdges, ctx, ctx.h)
					});
			};
			var _new = A2(
				$elm$core$Maybe$map,
				cleanUpEdges,
				updater(old));
			var diff = A2($elm_community$graph$Graph$computeEdgeDiff, old, _new);
			return A3(
				$elm_community$intdict$IntDict$update,
				nodeId,
				$elm$core$Basics$always(_new),
				A3($elm_community$graph$Graph$applyEdgeDiff, nodeId, diff, rep));
		};
		return A2(
			$elm$core$Basics$composeR,
			$elm_community$graph$Graph$unGraph,
			A2($elm$core$Basics$composeR, wrappedUpdater, $elm$core$Basics$identity));
	});
var $elm_community$graph$Graph$remove = F2(
	function (nodeId, graph) {
		return A3(
			$elm_community$graph$Graph$update,
			nodeId,
			$elm$core$Basics$always($elm$core$Maybe$Nothing),
			graph);
	});
var $elm_community$graph$Graph$guidedDfs = F5(
	function (selectNeighbors, visitNode, startingSeeds, startingAcc, startingGraph) {
		var go = F3(
			function (seeds, acc, graph) {
				go:
				while (true) {
					if (!seeds.b) {
						return _Utils_Tuple2(acc, graph);
					} else {
						var next = seeds.a;
						var seeds1 = seeds.b;
						var _v1 = A2($elm_community$graph$Graph$get, next, graph);
						if (_v1.$ === 1) {
							var $temp$seeds = seeds1,
								$temp$acc = acc,
								$temp$graph = graph;
							seeds = $temp$seeds;
							acc = $temp$acc;
							graph = $temp$graph;
							continue go;
						} else {
							var ctx = _v1.a;
							var _v2 = A2(visitNode, ctx, acc);
							var accAfterDiscovery = _v2.a;
							var finishNode = _v2.b;
							var _v3 = A3(
								go,
								selectNeighbors(ctx),
								accAfterDiscovery,
								A2($elm_community$graph$Graph$remove, next, graph));
							var accBeforeFinish = _v3.a;
							var graph1 = _v3.b;
							var accAfterFinish = finishNode(accBeforeFinish);
							var $temp$seeds = seeds1,
								$temp$acc = accAfterFinish,
								$temp$graph = graph1;
							seeds = $temp$seeds;
							acc = $temp$acc;
							graph = $temp$graph;
							continue go;
						}
					}
				}
			});
		return A3(go, startingSeeds, startingAcc, startingGraph);
	});
var $elm_community$graph$Graph$nodeIds = A2($elm$core$Basics$composeR, $elm_community$graph$Graph$unGraph, $elm_community$intdict$IntDict$keys);
var $elm_community$graph$Graph$dfs = F3(
	function (visitNode, acc, graph) {
		return A5(
			$elm_community$graph$Graph$guidedDfs,
			$elm_community$graph$Graph$alongOutgoingEdges,
			visitNode,
			$elm_community$graph$Graph$nodeIds(graph),
			acc,
			graph).a;
	});
var $elm_community$graph$Graph$onFinish = F3(
	function (visitor, ctx, acc) {
		return _Utils_Tuple2(
			acc,
			visitor(ctx));
	});
var $elm_community$graph$Graph$checkAcyclic = function (graph) {
	var reversePostOrder = A3(
		$elm_community$graph$Graph$dfs,
		$elm_community$graph$Graph$onFinish(
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.cP;
				},
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.L;
					},
					$elm$core$List$cons))),
		_List_Nil,
		graph);
	return A2($elm_community$graph$Graph$checkForBackEdges, reversePostOrder, graph);
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $stil4m$elm_syntax$Elm$Syntax$Range$Range = F2(
	function (start, end) {
		return {eq: end, fT: start};
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Range$compareLocations = F2(
	function (left, right) {
		return (_Utils_cmp(left.fD, right.fD) < 0) ? 0 : ((_Utils_cmp(right.fD, left.fD) < 0) ? 2 : A2($elm$core$Basics$compare, left.ec, right.ec));
	});
var $elm$core$List$sortWith = _List_sortWith;
var $stil4m$elm_syntax$Elm$Syntax$Range$sortLocations = $elm$core$List$sortWith($stil4m$elm_syntax$Elm$Syntax$Range$compareLocations);
var $stil4m$elm_syntax$Elm$Syntax$Range$combine = function (ranges) {
	var starts = $stil4m$elm_syntax$Elm$Syntax$Range$sortLocations(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.fT;
			},
			ranges));
	var ends = $elm$core$List$reverse(
		$stil4m$elm_syntax$Elm$Syntax$Range$sortLocations(
			A2(
				$elm$core$List$map,
				function ($) {
					return $.eq;
				},
				ranges)));
	return A2(
		$elm$core$Maybe$withDefault,
		$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
		A3(
			$elm$core$Maybe$map2,
			$stil4m$elm_syntax$Elm$Syntax$Range$Range,
			$elm$core$List$head(starts),
			$elm$core$List$head(ends)));
};
var $author$project$Morphir$Elm$Frontend$fixAssociativity = function (expr) {
	if ((expr.$ === 2) && (expr.d.b.$ === 2)) {
		var o = expr.a;
		var d = expr.b;
		var _v1 = expr.c;
		var lr = _v1.a;
		var l = _v1.b;
		var _v2 = expr.d;
		var _v3 = _v2.b;
		var ro = _v3.a;
		var rd = _v3.b;
		var _v4 = _v3.c;
		var rlr = _v4.a;
		var rl = _v4.b;
		var _v5 = _v3.d;
		var rrr = _v5.a;
		var rr = _v5.b;
		return (_Utils_eq(o, ro) && (!d)) ? $author$project$Morphir$Elm$Frontend$fixAssociativity(
			A4(
				$stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication,
				o,
				d,
				A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					$stil4m$elm_syntax$Elm$Syntax$Range$combine(
						_List_fromArray(
							[lr, rlr])),
					A4(
						$stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication,
						ro,
						rd,
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, lr, l),
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, rlr, rl))),
				A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, rrr, rr))) : expr;
	} else {
		return expr;
	}
};
var $elm_community$graph$Graph$NodeContext = F3(
	function (node, incoming, outgoing) {
		return {i: incoming, cP: node, h: outgoing};
	});
var $elm_community$graph$Graph$fromNodesAndEdges = F2(
	function (nodes_, edges_) {
		var nodeRep = A3(
			$elm$core$List$foldl,
			function (n) {
				return A2(
					$elm_community$intdict$IntDict$insert,
					n.L,
					A3($elm_community$graph$Graph$NodeContext, n, $elm_community$intdict$IntDict$empty, $elm_community$intdict$IntDict$empty));
			},
			$elm_community$intdict$IntDict$empty,
			nodes_);
		var addEdge = F2(
			function (edge, rep) {
				var updateOutgoing = function (ctx) {
					return _Utils_update(
						ctx,
						{
							h: A3($elm_community$intdict$IntDict$insert, edge.aD, edge.bp, ctx.h)
						});
				};
				var updateIncoming = function (ctx) {
					return _Utils_update(
						ctx,
						{
							i: A3($elm_community$intdict$IntDict$insert, edge.aA, edge.bp, ctx.i)
						});
				};
				return A3(
					$elm_community$intdict$IntDict$update,
					edge.aD,
					$elm$core$Maybe$map(updateIncoming),
					A3(
						$elm_community$intdict$IntDict$update,
						edge.aA,
						$elm$core$Maybe$map(updateOutgoing),
						rep));
			});
		var addEdgeIfValid = F2(
			function (edge, rep) {
				return (A2($elm_community$intdict$IntDict$member, edge.aA, rep) && A2($elm_community$intdict$IntDict$member, edge.aD, rep)) ? A2(addEdge, edge, rep) : rep;
			});
		return A3($elm$core$List$foldl, addEdgeIfValid, nodeRep, edges_);
	});
var $elm$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		if (ra.$ === 1) {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 1) {
				var x = rb.a;
				return $elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				if (rc.$ === 1) {
					var x = rc.a;
					return $elm$core$Result$Err(x);
				} else {
					var c = rc.a;
					return $elm$core$Result$Ok(
						A3(func, a, b, c));
				}
			}
		}
	});
var $author$project$Morphir$IR$SDK$Basics$add = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'add'));
};
var $author$project$Morphir$IR$SDK$Basics$and = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'and'));
};
var $author$project$Morphir$IR$SDK$Basics$composeLeft = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'composeLeft'));
};
var $author$project$Morphir$IR$SDK$Basics$composeRight = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'composeRight'));
};
var $author$project$Morphir$IR$SDK$List$construct = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$List$moduleName, 'construct'));
};
var $author$project$Morphir$IR$SDK$Basics$divide = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'divide'));
};
var $author$project$Morphir$IR$SDK$Basics$equal = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'equal'));
};
var $author$project$Morphir$IR$SDK$Basics$greaterThan = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'greaterThan'));
};
var $author$project$Morphir$IR$SDK$Basics$greaterThanOrEqual = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'greaterThanOrEqual'));
};
var $author$project$Morphir$IR$SDK$Basics$integerDivide = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'integerDivide'));
};
var $author$project$Morphir$IR$SDK$Basics$lessThan = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'lessThan'));
};
var $author$project$Morphir$IR$SDK$Basics$lessThanOrEqual = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'lessThanOrEqual'));
};
var $author$project$Morphir$IR$SDK$Basics$multiply = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'multiply'));
};
var $author$project$Morphir$IR$SDK$Basics$notEqual = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'notEqual'));
};
var $author$project$Morphir$IR$SDK$Basics$or = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'or'));
};
var $author$project$Morphir$IR$SDK$Basics$power = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'power'));
};
var $author$project$Morphir$IR$SDK$Basics$subtract = function (a) {
	return A2(
		$author$project$Morphir$IR$Value$Reference,
		a,
		A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'subtract'));
};
var $author$project$Morphir$Elm$Frontend$mapOperator = F2(
	function (sourceLocation, op) {
		switch (op) {
			case '||':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$or(sourceLocation));
			case '&&':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$and(sourceLocation));
			case '==':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$equal(sourceLocation));
			case '/=':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$notEqual(sourceLocation));
			case '<':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$lessThan(sourceLocation));
			case '>':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$greaterThan(sourceLocation));
			case '<=':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$lessThanOrEqual(sourceLocation));
			case '>=':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$greaterThanOrEqual(sourceLocation));
			case '++':
				return $elm$core$Result$Err(
					_List_fromArray(
						[
							A2($author$project$Morphir$Elm$Frontend$NotSupported, sourceLocation, 'The ++ operator is currently not supported. Please use String.append or List.append. See docs/error-append-not-supported.md')
						]));
			case '+':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$add(sourceLocation));
			case '-':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$subtract(sourceLocation));
			case '*':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$multiply(sourceLocation));
			case '/':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$divide(sourceLocation));
			case '//':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$integerDivide(sourceLocation));
			case '^':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$power(sourceLocation));
			case '<<':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$composeLeft(sourceLocation));
			case '>>':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$Basics$composeRight(sourceLocation));
			case '::':
				return $elm$core$Result$Ok(
					$author$project$Morphir$IR$SDK$List$construct(sourceLocation));
			default:
				return $elm$core$Result$Err(
					_List_fromArray(
						[
							A2($author$project$Morphir$Elm$Frontend$NotSupported, sourceLocation, 'OperatorApplication: ' + op)
						]));
		}
	});
var $author$project$Morphir$Elm$Frontend$RecordPatternNotSupported = function (a) {
	return {$: 8, a: a};
};
var $author$project$Morphir$Elm$Frontend$mapPattern = F2(
	function (sourceFile, _v0) {
		mapPattern:
		while (true) {
			var range = _v0.a;
			var pattern = _v0.b;
			var sourceLocation = A2($author$project$Morphir$Elm$Frontend$SourceLocation, sourceFile, range);
			switch (pattern.$) {
				case 0:
					return $elm$core$Result$Ok(
						$author$project$Morphir$IR$Value$WildcardPattern(sourceLocation));
				case 1:
					return $elm$core$Result$Ok(
						$author$project$Morphir$IR$Value$UnitPattern(sourceLocation));
				case 2:
					var _char = pattern.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$LiteralPattern,
							sourceLocation,
							$author$project$Morphir$IR$Literal$CharLiteral(_char)));
				case 3:
					var string = pattern.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$LiteralPattern,
							sourceLocation,
							$author$project$Morphir$IR$Literal$StringLiteral(string)));
				case 4:
					var _int = pattern.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$LiteralPattern,
							sourceLocation,
							$author$project$Morphir$IR$Literal$IntLiteral(_int)));
				case 5:
					var _int = pattern.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$LiteralPattern,
							sourceLocation,
							$author$project$Morphir$IR$Literal$IntLiteral(_int)));
				case 6:
					var _float = pattern.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$LiteralPattern,
							sourceLocation,
							$author$project$Morphir$IR$Literal$FloatLiteral(_float)));
				case 7:
					var elemNodes = pattern.a;
					return A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Value$TuplePattern(sourceLocation),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Elm$Frontend$mapPattern(sourceFile),
									elemNodes))));
				case 8:
					var fieldNameNodes = pattern.a;
					return $elm$core$Result$Err(
						_List_fromArray(
							[
								$author$project$Morphir$Elm$Frontend$RecordPatternNotSupported(sourceLocation)
							]));
				case 9:
					var headNode = pattern.a;
					var tailNode = pattern.b;
					return A3(
						$elm$core$Result$map2,
						$author$project$Morphir$IR$Value$HeadTailPattern(sourceLocation),
						A2($author$project$Morphir$Elm$Frontend$mapPattern, sourceFile, headNode),
						A2($author$project$Morphir$Elm$Frontend$mapPattern, sourceFile, tailNode));
				case 10:
					var itemNodes = pattern.a;
					var toPattern = function (patternNodes) {
						if (!patternNodes.b) {
							return $elm$core$Result$Ok(
								$author$project$Morphir$IR$Value$EmptyListPattern(sourceLocation));
						} else {
							var headNode = patternNodes.a;
							var tailNodes = patternNodes.b;
							return A3(
								$elm$core$Result$map2,
								$author$project$Morphir$IR$Value$HeadTailPattern(sourceLocation),
								A2($author$project$Morphir$Elm$Frontend$mapPattern, sourceFile, headNode),
								toPattern(tailNodes));
						}
					};
					return toPattern(itemNodes);
				case 11:
					var name = pattern.a;
					return $elm$core$Result$Ok(
						A3(
							$author$project$Morphir$IR$Value$AsPattern,
							sourceLocation,
							$author$project$Morphir$IR$Value$WildcardPattern(sourceLocation),
							$author$project$Morphir$IR$Name$fromString(name)));
				case 12:
					var qualifiedNameRef = pattern.a;
					var argNodes = pattern.b;
					var qualifiedName = A2(
						$author$project$Morphir$IR$FQName$fromQName,
						_List_Nil,
						A2(
							$author$project$Morphir$IR$QName$fromName,
							A2($elm$core$List$map, $author$project$Morphir$IR$Name$fromString, qualifiedNameRef.bs),
							$author$project$Morphir$IR$Name$fromString(qualifiedNameRef.cN)));
					return A2(
						$elm$core$Result$map,
						A2($author$project$Morphir$IR$Value$ConstructorPattern, sourceLocation, qualifiedName),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Elm$Frontend$mapPattern(sourceFile),
									argNodes))));
				case 13:
					var subjectNode = pattern.a;
					var aliasNode = pattern.b;
					return A2(
						$elm$core$Result$map,
						function (subject) {
							return A3(
								$author$project$Morphir$IR$Value$AsPattern,
								sourceLocation,
								subject,
								$author$project$Morphir$IR$Name$fromString(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(aliasNode)));
						},
						A2($author$project$Morphir$Elm$Frontend$mapPattern, sourceFile, subjectNode));
				default:
					var childNode = pattern.a;
					var $temp$sourceFile = sourceFile,
						$temp$_v0 = childNode;
					sourceFile = $temp$sourceFile;
					_v0 = $temp$_v0;
					continue mapPattern;
			}
		}
	});
var $author$project$Morphir$Elm$Frontend$namesBoundByPattern = function (p) {
	var namesBound = function (pattern) {
		namesBound:
		while (true) {
			switch (pattern.$) {
				case 7:
					var elemPatternNodes = pattern.a;
					return A2(
						$elm$core$List$concatMap,
						A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, namesBound),
						elemPatternNodes);
				case 8:
					var fieldNameNodes = pattern.a;
					return A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, fieldNameNodes);
				case 9:
					var _v1 = pattern.a;
					var headPattern = _v1.b;
					var _v2 = pattern.b;
					var tailPattern = _v2.b;
					return _Utils_ap(
						namesBound(headPattern),
						namesBound(tailPattern));
				case 10:
					var itemPatternNodes = pattern.a;
					return A2(
						$elm$core$List$concatMap,
						A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, namesBound),
						itemPatternNodes);
				case 11:
					var name = pattern.a;
					return _List_fromArray(
						[name]);
				case 12:
					var argPatternNodes = pattern.b;
					return A2(
						$elm$core$List$concatMap,
						A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, namesBound),
						argPatternNodes);
				case 13:
					var _v3 = pattern.a;
					var childPattern = _v3.b;
					var _v4 = pattern.b;
					var alias = _v4.b;
					return A2(
						$elm$core$List$cons,
						alias,
						namesBound(childPattern));
				case 14:
					var _v5 = pattern.a;
					var childPattern = _v5.b;
					var $temp$pattern = childPattern;
					pattern = $temp$pattern;
					continue namesBound;
				default:
					return _List_Nil;
			}
		}
	};
	return $elm$core$Set$fromList(
		namesBound(p));
};
var $author$project$Morphir$IR$SDK$Basics$negate = F3(
	function (refAttributes, valueAttributes, arg) {
		return A3(
			$author$project$Morphir$IR$Value$Apply,
			valueAttributes,
			A2(
				$author$project$Morphir$IR$Value$Reference,
				refAttributes,
				A2($author$project$Morphir$IR$SDK$Common$toFQName, $author$project$Morphir$IR$SDK$Basics$moduleName, 'negate')),
			arg);
	});
var $elm_community$intdict$IntDict$values = function (dict) {
	return A3(
		$elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $elm_community$graph$Graph$nodes = A2(
	$elm$core$Basics$composeR,
	$elm_community$graph$Graph$unGraph,
	A2(
		$elm$core$Basics$composeR,
		$elm_community$intdict$IntDict$values,
		$elm$core$List$map(
			function ($) {
				return $.cP;
			})));
var $elm_community$graph$Graph$Tree$MkTree = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm_community$graph$Graph$Tree$empty = A2($elm_community$graph$Graph$Tree$MkTree, 0, $elm$core$Maybe$Nothing);
var $elm_community$graph$Graph$Tree$isEmpty = function (tree) {
	return _Utils_eq(tree, $elm_community$graph$Graph$Tree$empty);
};
var $elm_community$graph$Graph$Tree$size = function (tree) {
	var n = tree.a;
	return n;
};
var $elm_community$graph$Graph$Tree$inner = F2(
	function (label, children) {
		var nonEmptyChildren = A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm_community$graph$Graph$Tree$isEmpty),
			children);
		var totalSize = A3(
			$elm$core$List$foldl,
			A2($elm$core$Basics$composeL, $elm$core$Basics$add, $elm_community$graph$Graph$Tree$size),
			1,
			nonEmptyChildren);
		return A2(
			$elm_community$graph$Graph$Tree$MkTree,
			totalSize,
			$elm$core$Maybe$Just(
				_Utils_Tuple2(label, nonEmptyChildren)));
	});
var $elm_community$graph$Graph$dfsForest = F2(
	function (seeds, graph) {
		var visitNode = F2(
			function (ctx, trees) {
				return _Utils_Tuple2(
					_List_Nil,
					function (children) {
						return A2(
							$elm$core$List$cons,
							A2($elm_community$graph$Graph$Tree$inner, ctx, children),
							trees);
					});
			});
		return $elm$core$List$reverse(
			A5($elm_community$graph$Graph$guidedDfs, $elm_community$graph$Graph$alongOutgoingEdges, visitNode, seeds, _List_Nil, graph).a);
	});
var $elm_community$graph$Graph$empty = $elm_community$intdict$IntDict$empty;
var $elm_community$graph$Graph$insert = F2(
	function (nodeContext, graph) {
		return A3(
			$elm_community$graph$Graph$update,
			nodeContext.cP.L,
			$elm$core$Basics$always(
				$elm$core$Maybe$Just(nodeContext)),
			graph);
	});
var $elm_community$graph$Graph$Tree$listForTraversal = F2(
	function (traversal, tree) {
		var f = F3(
			function (label, children, rest) {
				return A2(
					$elm$core$Basics$composeR,
					$elm$core$List$cons(label),
					rest);
			});
		var acc = $elm$core$Basics$identity;
		return A4(traversal, f, acc, tree, _List_Nil);
	});
var $elm_community$graph$Graph$Tree$root = function (tree) {
	var maybe = tree.b;
	return maybe;
};
var $elm_community$graph$Graph$Tree$preOrder = F3(
	function (visit, acc, tree) {
		var folder = F2(
			function (b, a) {
				return A3($elm_community$graph$Graph$Tree$preOrder, visit, a, b);
			});
		var _v0 = $elm_community$graph$Graph$Tree$root(tree);
		if (_v0.$ === 1) {
			return acc;
		} else {
			var _v1 = _v0.a;
			var label = _v1.a;
			var children = _v1.b;
			return A3(
				$elm$core$List$foldl,
				folder,
				A3(visit, label, children, acc),
				children);
		}
	});
var $elm_community$graph$Graph$Tree$preOrderList = $elm_community$graph$Graph$Tree$listForTraversal($elm_community$graph$Graph$Tree$preOrder);
var $elm_community$intdict$IntDict$map = F2(
	function (f, dict) {
		switch (dict.$) {
			case 0:
				return $elm_community$intdict$IntDict$empty;
			case 1:
				var l = dict.a;
				return A2(
					$elm_community$intdict$IntDict$leaf,
					l.cG,
					A2(f, l.cG, l.aT));
			default:
				var i = dict.a;
				return A3(
					$elm_community$intdict$IntDict$inner,
					i.g,
					A2($elm_community$intdict$IntDict$map, f, i.c),
					A2($elm_community$intdict$IntDict$map, f, i.d));
		}
	});
var $elm_community$graph$Graph$reverseEdges = function () {
	var updateContext = F2(
		function (nodeId, ctx) {
			return _Utils_update(
				ctx,
				{i: ctx.h, h: ctx.i});
		});
	return A2(
		$elm$core$Basics$composeR,
		$elm_community$graph$Graph$unGraph,
		A2(
			$elm$core$Basics$composeR,
			$elm_community$intdict$IntDict$map(updateContext),
			$elm$core$Basics$identity));
}();
var $elm_community$graph$Graph$stronglyConnectedComponents = function (graph) {
	var reversePostOrder = A3(
		$elm_community$graph$Graph$dfs,
		$elm_community$graph$Graph$onFinish(
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.cP;
				},
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.L;
					},
					$elm$core$List$cons))),
		_List_Nil,
		graph);
	return A2(
		$elm$core$Result$mapError,
		function (_v0) {
			var forest = A2(
				$elm_community$graph$Graph$dfsForest,
				reversePostOrder,
				$elm_community$graph$Graph$reverseEdges(graph));
			return A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					$elm_community$graph$Graph$Tree$preOrderList,
					A2(
						$elm$core$Basics$composeR,
						A2($elm$core$List$foldr, $elm_community$graph$Graph$insert, $elm_community$graph$Graph$empty),
						$elm_community$graph$Graph$reverseEdges)),
				forest);
		},
		A2($elm_community$graph$Graph$checkForBackEdges, reversePostOrder, graph));
};
var $elm_community$graph$Graph$topologicalSort = function (_v0) {
	var graph = _v0.a;
	var ordering = _v0.b;
	var error = 'Graph.topologicalSort: Invalid `AcyclicGraph`, where the ordering contained nodes not present in the graph';
	return A2(
		$elm$core$List$map,
		function (id) {
			return A3($elm_community$graph$Graph$unsafeGet, error, id, graph);
		},
		ordering);
};
var $author$project$Morphir$Elm$Frontend$mapExpression = F2(
	function (sourceFile, _v43) {
		mapExpression:
		while (true) {
			var range = _v43.a;
			var exp = _v43.b;
			var sourceLocation = A2($author$project$Morphir$Elm$Frontend$SourceLocation, sourceFile, range);
			var _v44 = $author$project$Morphir$Elm$Frontend$fixAssociativity(exp);
			switch (_v44.$) {
				case 0:
					return $elm$core$Result$Ok(
						$author$project$Morphir$IR$Value$Unit(sourceLocation));
				case 1:
					var expNodes = _v44.a;
					var toApply = function (valuesReversed) {
						if (!valuesReversed.b) {
							return $elm$core$Result$Err(
								_List_fromArray(
									[
										$author$project$Morphir$Elm$Frontend$EmptyApply(sourceLocation)
									]));
						} else {
							if (!valuesReversed.b.b) {
								var singleValue = valuesReversed.a;
								return $elm$core$Result$Ok(singleValue);
							} else {
								var lastValue = valuesReversed.a;
								var restOfValuesReversed = valuesReversed.b;
								return A2(
									$elm$core$Result$map,
									function (funValue) {
										return A3($author$project$Morphir$IR$Value$Apply, sourceLocation, funValue, lastValue);
									},
									toApply(restOfValuesReversed));
							}
						}
					};
					return A2(
						$elm$core$Result$andThen,
						A2($elm$core$Basics$composeR, $elm$core$List$reverse, toApply),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Elm$Frontend$mapExpression(sourceFile),
									expNodes))));
				case 2:
					var op = _v44.a;
					var leftNode = _v44.c;
					var rightNode = _v44.d;
					switch (op) {
						case '<|':
							return A3(
								$elm$core$Result$map2,
								$author$project$Morphir$IR$Value$Apply(sourceLocation),
								A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, leftNode),
								A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, rightNode));
						case '|>':
							return A3(
								$elm$core$Result$map2,
								$author$project$Morphir$IR$Value$Apply(sourceLocation),
								A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, rightNode),
								A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, leftNode));
						default:
							return A4(
								$elm$core$Result$map3,
								F3(
									function (fun, arg1, arg2) {
										return A3(
											$author$project$Morphir$IR$Value$Apply,
											sourceLocation,
											A3($author$project$Morphir$IR$Value$Apply, sourceLocation, fun, arg1),
											arg2);
									}),
								A2($author$project$Morphir$Elm$Frontend$mapOperator, sourceLocation, op),
								A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, leftNode),
								A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, rightNode));
					}
				case 3:
					var moduleName = _v44.a;
					var localName = _v44.b;
					return A2(
						$elm$core$Result$andThen,
						function (_v47) {
							var firstChar = _v47.a;
							if ($elm$core$Char$isUpper(firstChar)) {
								var _v48 = _Utils_Tuple2(moduleName, localName);
								_v48$2:
								while (true) {
									if (!_v48.a.b) {
										switch (_v48.b) {
											case 'True':
												return $elm$core$Result$Ok(
													A2(
														$author$project$Morphir$IR$Value$Literal,
														sourceLocation,
														$author$project$Morphir$IR$Literal$BoolLiteral(true)));
											case 'False':
												return $elm$core$Result$Ok(
													A2(
														$author$project$Morphir$IR$Value$Literal,
														sourceLocation,
														$author$project$Morphir$IR$Literal$BoolLiteral(false)));
											default:
												break _v48$2;
										}
									} else {
										break _v48$2;
									}
								}
								return $elm$core$Result$Ok(
									A2(
										$author$project$Morphir$IR$Value$Constructor,
										sourceLocation,
										A3(
											$author$project$Morphir$IR$FQName$fQName,
											_List_Nil,
											A2($elm$core$List$map, $author$project$Morphir$IR$Name$fromString, moduleName),
											$author$project$Morphir$IR$Name$fromString(localName))));
							} else {
								return $elm$core$Result$Ok(
									A2(
										$author$project$Morphir$IR$Value$Reference,
										sourceLocation,
										A3(
											$author$project$Morphir$IR$FQName$fQName,
											_List_Nil,
											A2($elm$core$List$map, $author$project$Morphir$IR$Name$fromString, moduleName),
											$author$project$Morphir$IR$Name$fromString(localName))));
							}
						},
						A2(
							$elm$core$Result$fromMaybe,
							_List_fromArray(
								[
									A2($author$project$Morphir$Elm$Frontend$NotSupported, sourceLocation, 'Empty value name')
								]),
							$elm$core$String$uncons(localName)));
				case 4:
					var condNode = _v44.a;
					var thenNode = _v44.b;
					var elseNode = _v44.c;
					return A4(
						$elm$core$Result$map3,
						$author$project$Morphir$IR$Value$IfThenElse(sourceLocation),
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, condNode),
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, thenNode),
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, elseNode));
				case 5:
					var op = _v44.a;
					return A2($author$project$Morphir$Elm$Frontend$mapOperator, sourceLocation, op);
				case 6:
					var op = _v44.a;
					return A2($author$project$Morphir$Elm$Frontend$mapOperator, sourceLocation, op);
				case 7:
					var value = _v44.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$Literal,
							sourceLocation,
							$author$project$Morphir$IR$Literal$IntLiteral(value)));
				case 8:
					var value = _v44.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$Literal,
							sourceLocation,
							$author$project$Morphir$IR$Literal$IntLiteral(value)));
				case 9:
					var value = _v44.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$Literal,
							sourceLocation,
							$author$project$Morphir$IR$Literal$FloatLiteral(value)));
				case 10:
					var arg = _v44.a;
					return A2(
						$elm$core$Result$map,
						A2($author$project$Morphir$IR$SDK$Basics$negate, sourceLocation, sourceLocation),
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, arg));
				case 11:
					var value = _v44.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$Literal,
							sourceLocation,
							$author$project$Morphir$IR$Literal$StringLiteral(value)));
				case 12:
					var value = _v44.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$Literal,
							sourceLocation,
							$author$project$Morphir$IR$Literal$CharLiteral(value)));
				case 13:
					var expNodes = _v44.a;
					return A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Value$Tuple(sourceLocation),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Elm$Frontend$mapExpression(sourceFile),
									expNodes))));
				case 14:
					var expNode = _v44.a;
					var $temp$sourceFile = sourceFile,
						$temp$_v43 = expNode;
					sourceFile = $temp$sourceFile;
					_v43 = $temp$_v43;
					continue mapExpression;
				case 15:
					var letBlock = _v44.a;
					return A3($author$project$Morphir$Elm$Frontend$mapLetExpression, sourceFile, sourceLocation, letBlock);
				case 16:
					var caseBlock = _v44.a;
					return A3(
						$elm$core$Result$map2,
						$author$project$Morphir$IR$Value$PatternMatch(sourceLocation),
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, caseBlock._),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									function (_v49) {
										var patternNode = _v49.a;
										var bodyNode = _v49.b;
										return A3(
											$elm$core$Result$map2,
											$elm$core$Tuple$pair,
											A2($author$project$Morphir$Elm$Frontend$mapPattern, sourceFile, patternNode),
											A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, bodyNode));
									},
									caseBlock.b8))));
				case 17:
					var lambda = _v44.a;
					var curriedLambda = F2(
						function (argNodes, bodyNode) {
							if (!argNodes.b) {
								return A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, bodyNode);
							} else {
								var firstArgNode = argNodes.a;
								var restOfArgNodes = argNodes.b;
								return A3(
									$elm$core$Result$map2,
									$author$project$Morphir$IR$Value$Lambda(sourceLocation),
									A2($author$project$Morphir$Elm$Frontend$mapPattern, sourceFile, firstArgNode),
									A2(curriedLambda, restOfArgNodes, bodyNode));
							}
						});
					return A2(curriedLambda, lambda.dP, lambda._);
				case 18:
					var fieldNodes = _v44.a;
					return A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Value$Record(sourceLocation),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									function (_v51) {
										var _v52 = _v51.a;
										var fieldName = _v52.b;
										var fieldValue = _v51.b;
										return A2(
											$elm$core$Result$map,
											$elm$core$Tuple$pair(
												$author$project$Morphir$IR$Name$fromString(fieldName)),
											A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, fieldValue));
									},
									A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, fieldNodes)))));
				case 19:
					var itemNodes = _v44.a;
					return A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Value$List(sourceLocation),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Elm$Frontend$mapExpression(sourceFile),
									itemNodes))));
				case 20:
					var targetNode = _v44.a;
					var fieldNameNode = _v44.b;
					return A2(
						$elm$core$Result$map,
						function (subjectValue) {
							return A3(
								$author$project$Morphir$IR$Value$Field,
								sourceLocation,
								subjectValue,
								$author$project$Morphir$IR$Name$fromString(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(fieldNameNode)));
						},
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, targetNode));
				case 21:
					var fieldName = _v44.a;
					return $elm$core$Result$Ok(
						A2(
							$author$project$Morphir$IR$Value$FieldFunction,
							sourceLocation,
							$author$project$Morphir$IR$Name$fromString(fieldName)));
				case 22:
					var targetVarNameNode = _v44.a;
					var fieldNodes = _v44.b;
					return A2(
						$elm$core$Result$map,
						A2(
							$author$project$Morphir$IR$Value$UpdateRecord,
							sourceLocation,
							A2(
								$author$project$Morphir$IR$Value$Variable,
								sourceLocation,
								$author$project$Morphir$IR$Name$fromString(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(targetVarNameNode)))),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									function (_v53) {
										var _v54 = _v53.a;
										var fieldName = _v54.b;
										var fieldValue = _v53.b;
										return A2(
											$elm$core$Result$map,
											$elm$core$Tuple$pair(
												$author$project$Morphir$IR$Name$fromString(fieldName)),
											A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, fieldValue));
									},
									A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, fieldNodes)))));
				default:
					return $elm$core$Result$Err(
						_List_fromArray(
							[
								A2($author$project$Morphir$Elm$Frontend$NotSupported, sourceLocation, 'GLSLExpression')
							]));
			}
		}
	});
var $author$project$Morphir$Elm$Frontend$mapFunction = F2(
	function (sourceFile, _v40) {
		var range = _v40.a;
		var _function = _v40.b;
		var valueTypeResult = function () {
			var _v41 = _function.fL;
			if (!_v41.$) {
				var _v42 = _v41.a;
				var signature = _v42.b;
				return A2($author$project$Morphir$Elm$Frontend$mapTypeAnnotation, sourceFile, signature.dn);
			} else {
				return $elm$core$Result$Err(
					_List_fromArray(
						[
							$author$project$Morphir$Elm$Frontend$MissingTypeSignature(
							A2($author$project$Morphir$Elm$Frontend$SourceLocation, sourceFile, range))
						]));
			}
		}();
		return A2(
			$elm$core$Result$andThen,
			function (valueType) {
				return function (funImpl) {
					return A4($author$project$Morphir$Elm$Frontend$mapFunctionImplementation, sourceFile, valueType, funImpl.b4, funImpl._);
				}(
					$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am));
			},
			valueTypeResult);
	});
var $author$project$Morphir$Elm$Frontend$mapFunctionImplementation = F4(
	function (sourceFile, valueType, argumentNodes, expression) {
		var sourceLocation = function (range) {
			return A2($author$project$Morphir$Elm$Frontend$SourceLocation, sourceFile, range);
		};
		var extractNamedParams = F3(
			function (namedParams, patternParams, restOfTypeSignature) {
				extractNamedParams:
				while (true) {
					var _v32 = _Utils_Tuple2(patternParams, restOfTypeSignature);
					if (!_v32.a.b) {
						return _Utils_Tuple3(namedParams, restOfTypeSignature, patternParams);
					} else {
						if (_v32.b.$ === 5) {
							var _v33 = _v32.a;
							var _v34 = _v33.a;
							var range = _v34.a;
							var firstParam = _v34.b;
							var restOfParams = _v33.b;
							var _v35 = _v32.b;
							var inType = _v35.b;
							var outType = _v35.c;
							if (firstParam.$ === 11) {
								var paramName = firstParam.a;
								var $temp$namedParams = _Utils_ap(
									namedParams,
									_List_fromArray(
										[
											_Utils_Tuple3(
											$author$project$Morphir$IR$Name$fromString(paramName),
											A2($author$project$Morphir$Elm$Frontend$SourceLocation, sourceFile, range),
											inType)
										])),
									$temp$patternParams = restOfParams,
									$temp$restOfTypeSignature = outType;
								namedParams = $temp$namedParams;
								patternParams = $temp$patternParams;
								restOfTypeSignature = $temp$restOfTypeSignature;
								continue extractNamedParams;
							} else {
								return _Utils_Tuple3(namedParams, restOfTypeSignature, patternParams);
							}
						} else {
							return _Utils_Tuple3(namedParams, restOfTypeSignature, patternParams);
						}
					}
				}
			});
		var _v37 = A3(extractNamedParams, _List_Nil, argumentNodes, valueType);
		var inputTypes = _v37.a;
		var outputType = _v37.b;
		var lambdaArgPatterns = _v37.c;
		var bodyResult = function () {
			var lambdaWithParams = F2(
				function (params, body) {
					if (!params.b) {
						return A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, body);
					} else {
						var _v39 = params.a;
						var range = _v39.a;
						var firstParam = _v39.b;
						var restOfParams = params.b;
						return A3(
							$elm$core$Result$map2,
							F2(
								function (lambdaArg, lambdaBody) {
									return A3(
										$author$project$Morphir$IR$Value$Lambda,
										sourceLocation(range),
										lambdaArg,
										lambdaBody);
								}),
							A2(
								$author$project$Morphir$Elm$Frontend$mapPattern,
								sourceFile,
								A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, firstParam)),
							A2(lambdaWithParams, restOfParams, body));
					}
				});
			return A2(lambdaWithParams, lambdaArgPatterns, expression);
		}();
		return A2(
			$elm$core$Result$map,
			A2($author$project$Morphir$IR$Value$Definition, inputTypes, outputType),
			bodyResult);
	});
var $author$project$Morphir$Elm$Frontend$mapLetExpression = F3(
	function (sourceFile, sourceLocation, letBlock) {
		var namesReferredByExpression = function (expression) {
			namesReferredByExpression:
			while (true) {
				_v0$14:
				while (true) {
					switch (expression.$) {
						case 1:
							var argNodes = expression.a;
							return A2(
								$elm$core$List$concatMap,
								A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, namesReferredByExpression),
								argNodes);
						case 2:
							var _v1 = expression.c;
							var leftExp = _v1.b;
							var _v2 = expression.d;
							var rightExp = _v2.b;
							return _Utils_ap(
								namesReferredByExpression(leftExp),
								namesReferredByExpression(rightExp));
						case 3:
							if (!expression.a.b) {
								var name = expression.b;
								return _List_fromArray(
									[name]);
							} else {
								break _v0$14;
							}
						case 4:
							var _v3 = expression.a;
							var condExp = _v3.b;
							var _v4 = expression.b;
							var thenExp = _v4.b;
							var _v5 = expression.c;
							var elseExp = _v5.b;
							return _Utils_ap(
								namesReferredByExpression(condExp),
								_Utils_ap(
									namesReferredByExpression(thenExp),
									namesReferredByExpression(elseExp)));
						case 10:
							var _v6 = expression.a;
							var childExp = _v6.b;
							var $temp$expression = childExp;
							expression = $temp$expression;
							continue namesReferredByExpression;
						case 13:
							var argNodes = expression.a;
							return A2(
								$elm$core$List$concatMap,
								A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, namesReferredByExpression),
								argNodes);
						case 14:
							var _v7 = expression.a;
							var childExp = _v7.b;
							var $temp$expression = childExp;
							expression = $temp$expression;
							continue namesReferredByExpression;
						case 15:
							var innerLetBlock = expression.a;
							return _Utils_ap(
								namesReferredByExpression(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(innerLetBlock._)),
								A2(
									$elm$core$List$concatMap,
									function (_v8) {
										var decl = _v8.b;
										if (!decl.$) {
											var _function = decl.a;
											return namesReferredByExpression(
												$stil4m$elm_syntax$Elm$Syntax$Node$value(
													$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am)._));
										} else {
											var _v10 = decl.b;
											var childExp = _v10.b;
											return namesReferredByExpression(childExp);
										}
									},
									innerLetBlock.a0));
						case 16:
							var caseBlock = expression.a;
							return _Utils_ap(
								namesReferredByExpression(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(caseBlock._)),
								A2(
									$elm$core$List$concatMap,
									function (_v11) {
										var _v12 = _v11.b;
										var childExp = _v12.b;
										return namesReferredByExpression(childExp);
									},
									caseBlock.b8));
						case 17:
							var lambda = expression.a;
							return namesReferredByExpression(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(lambda._));
						case 18:
							var setterNodes = expression.a;
							return A2(
								$elm$core$List$concatMap,
								function (_v13) {
									var _v14 = _v13.b;
									var _v15 = _v14.b;
									var childExp = _v15.b;
									return namesReferredByExpression(childExp);
								},
								setterNodes);
						case 19:
							var argNodes = expression.a;
							return A2(
								$elm$core$List$concatMap,
								A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, namesReferredByExpression),
								argNodes);
						case 20:
							var _v16 = expression.a;
							var childExp = _v16.b;
							var $temp$expression = childExp;
							expression = $temp$expression;
							continue namesReferredByExpression;
						case 22:
							var _v17 = expression.a;
							var recordRef = _v17.b;
							var setterNodes = expression.b;
							return A2(
								$elm$core$List$cons,
								recordRef,
								A2(
									$elm$core$List$concatMap,
									function (_v18) {
										var _v19 = _v18.b;
										var _v20 = _v19.b;
										var childExp = _v20.b;
										return namesReferredByExpression(childExp);
									},
									setterNodes));
						default:
							break _v0$14;
					}
				}
				return _List_Nil;
			}
		};
		var letBlockToValue = F2(
			function (declarationNodes, inNode) {
				var letDeclarationToValue = F2(
					function (letDeclarationNode, valueResult) {
						if (!letDeclarationNode.b.$) {
							var range = letDeclarationNode.a;
							var _function = letDeclarationNode.b.a;
							return A3(
								$elm$core$Result$map2,
								A2(
									$author$project$Morphir$IR$Value$LetDefinition,
									sourceLocation,
									$author$project$Morphir$IR$Name$fromString(
										$stil4m$elm_syntax$Elm$Syntax$Node$value(
											$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am).cN))),
								A2(
									$author$project$Morphir$Elm$Frontend$mapFunction,
									sourceFile,
									A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, _function)),
								valueResult);
						} else {
							var range = letDeclarationNode.a;
							var _v31 = letDeclarationNode.b;
							var patternNode = _v31.a;
							var letExpressionNode = _v31.b;
							return A4(
								$elm$core$Result$map3,
								$author$project$Morphir$IR$Value$Destructure(sourceLocation),
								A2($author$project$Morphir$Elm$Frontend$mapPattern, sourceFile, patternNode),
								A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, letExpressionNode),
								valueResult);
						}
					});
				var declarationIndexForName = $elm$core$Dict$fromList(
					$elm$core$List$concat(
						A2(
							$elm$core$List$indexedMap,
							F2(
								function (index, _v27) {
									var decl = _v27.b;
									if (!decl.$) {
										var _function = decl.a;
										return _List_fromArray(
											[
												_Utils_Tuple2(
												$stil4m$elm_syntax$Elm$Syntax$Node$value(
													$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am).cN),
												index)
											]);
									} else {
										var _v29 = decl.a;
										var pattern = _v29.b;
										return $elm$core$Set$toList(
											A2(
												$elm$core$Set$map,
												function (name) {
													return _Utils_Tuple2(name, index);
												},
												$author$project$Morphir$Elm$Frontend$namesBoundByPattern(pattern)));
									}
								}),
							declarationNodes)));
				var declarationDependencyGraph = function () {
					var nodes = A2(
						$elm$core$List$indexedMap,
						F2(
							function (index, declNode) {
								return A2($elm_community$graph$Graph$Node, index, declNode);
							}),
						declarationNodes);
					var edges = $elm$core$List$concat(
						A2(
							$elm$core$List$indexedMap,
							F2(
								function (fromIndex, _v25) {
									var decl = _v25.b;
									if (!decl.$) {
										var _function = decl.a;
										return A2(
											$elm$core$List$filterMap,
											function (name) {
												return A2(
													$elm$core$Maybe$map,
													function (toIndex) {
														return A3($elm_community$graph$Graph$Edge, fromIndex, toIndex, name);
													},
													A2($elm$core$Dict$get, name, declarationIndexForName));
											},
											namesReferredByExpression(
												$stil4m$elm_syntax$Elm$Syntax$Node$value(
													$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am)._)));
									} else {
										var expression = decl.b;
										return A2(
											$elm$core$List$filterMap,
											function (name) {
												return A2(
													$elm$core$Maybe$map,
													function (toIndex) {
														return A3($elm_community$graph$Graph$Edge, fromIndex, toIndex, name);
													},
													A2($elm$core$Dict$get, name, declarationIndexForName));
											},
											namesReferredByExpression(
												$stil4m$elm_syntax$Elm$Syntax$Node$value(expression)));
									}
								}),
							declarationNodes));
					return A2($elm_community$graph$Graph$fromNodesAndEdges, nodes, edges);
				}();
				var componentGraphToValue = F2(
					function (componentGraph, valueResult) {
						var _v22 = $elm_community$graph$Graph$checkAcyclic(componentGraph);
						if (!_v22.$) {
							var acyclic = _v22.a;
							return A3(
								$elm$core$List$foldl,
								F2(
									function (nodeContext, innerSoFar) {
										return A2(letDeclarationToValue, nodeContext.cP.bp, innerSoFar);
									}),
								valueResult,
								$elm_community$graph$Graph$topologicalSort(acyclic));
						} else {
							return A3(
								$elm$core$Result$map2,
								$author$project$Morphir$IR$Value$LetRecursion(sourceLocation),
								A2(
									$elm$core$Result$map,
									$elm$core$Dict$fromList,
									A2(
										$elm$core$Result$mapError,
										$elm$core$List$concat,
										$author$project$Morphir$ListOfResults$liftAllErrors(
											A2(
												$elm$core$List$map,
												function (graphNode) {
													var _v23 = graphNode.bp;
													if (!_v23.b.$) {
														var range = _v23.a;
														var _function = _v23.b.a;
														return A2(
															$elm$core$Result$map,
															$elm$core$Tuple$pair(
																$author$project$Morphir$IR$Name$fromString(
																	$stil4m$elm_syntax$Elm$Syntax$Node$value(
																		$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am).cN))),
															A2(
																$author$project$Morphir$Elm$Frontend$mapFunction,
																sourceFile,
																A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, _function)));
													} else {
														var range = _v23.a;
														var _v24 = _v23.b;
														return $elm$core$Result$Err(
															_List_fromArray(
																[
																	A2($author$project$Morphir$Elm$Frontend$NotSupported, sourceLocation, 'Recursive destructuring')
																]));
													}
												},
												$elm_community$graph$Graph$nodes(componentGraph))))),
								valueResult);
						}
					});
				var _v21 = $elm_community$graph$Graph$stronglyConnectedComponents(declarationDependencyGraph);
				if (!_v21.$) {
					var acyclic = _v21.a;
					return A3(
						$elm$core$List$foldl,
						F2(
							function (nodeContext, soFar) {
								return A2(letDeclarationToValue, nodeContext.cP.bp, soFar);
							}),
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, inNode),
						$elm_community$graph$Graph$topologicalSort(acyclic));
				} else {
					var components = _v21.a;
					return A3(
						$elm$core$List$foldl,
						componentGraphToValue,
						A2($author$project$Morphir$Elm$Frontend$mapExpression, sourceFile, inNode),
						components);
				}
			});
		return A2(letBlockToValue, letBlock.a0, letBlock._);
	});
var $author$project$Morphir$Elm$Frontend$mapDeclarationsToValue = F3(
	function (sourceFile, expose, decls) {
		return A2(
			$elm$core$Result$mapError,
			$elm$core$List$concat,
			$author$project$Morphir$ListOfResults$liftAllErrors(
				A2(
					$elm$core$List$filterMap,
					function (_v0) {
						var range = _v0.a;
						var decl = _v0.b;
						if (!decl.$) {
							var _function = decl.a;
							var valueName = $author$project$Morphir$IR$Name$fromString(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am).cN));
							var valueDef = A2(
								$elm$core$Result$map,
								$author$project$Morphir$IR$AccessControlled$public,
								A2(
									$author$project$Morphir$Elm$Frontend$mapFunction,
									sourceFile,
									A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, _function)));
							return $elm$core$Maybe$Just(
								A2(
									$elm$core$Result$map,
									$elm$core$Tuple$pair(valueName),
									valueDef));
						} else {
							return $elm$core$Maybe$Nothing;
						}
					},
					decls)));
	});
var $author$project$Morphir$IR$Type$mapDefinition = F2(
	function (f, def) {
		if (!def.$) {
			var params = def.a;
			var tpe = def.b;
			return A2(
				$elm$core$Result$mapError,
				$elm$core$List$singleton,
				A2(
					$elm$core$Result$map,
					$author$project$Morphir$IR$Type$TypeAliasDefinition(params),
					f(tpe)));
		} else {
			var params = def.a;
			var constructors = def.b;
			var ctorsResult = A2(
				$elm$core$Result$mapError,
				$elm$core$List$concat,
				A2(
					$elm$core$Result$map,
					$author$project$Morphir$IR$AccessControlled$AccessControlled(constructors.b0),
					$author$project$Morphir$ListOfResults$liftAllErrors(
						A2(
							$elm$core$List$map,
							function (_v1) {
								var ctorName = _v1.a;
								var ctorArgs = _v1.b;
								return A2(
									$elm$core$Result$map,
									$author$project$Morphir$IR$Type$Constructor(ctorName),
									$author$project$Morphir$ListOfResults$liftAllErrors(
										A2(
											$elm$core$List$map,
											function (_v2) {
												var argName = _v2.a;
												var argType = _v2.b;
												return A2(
													$elm$core$Result$map,
													$elm$core$Tuple$pair(argName),
													f(argType));
											},
											ctorArgs)));
							},
							constructors.aT))));
			return A2(
				$elm$core$Result$map,
				$author$project$Morphir$IR$Type$CustomTypeDefinition(params),
				ctorsResult);
		}
	});
var $author$project$Morphir$IR$Value$mapDefinition = F3(
	function (mapType, mapValue, def) {
		return A4(
			$elm$core$Result$map3,
			F3(
				function (inputTypes, outputType, body) {
					return A3($author$project$Morphir$IR$Value$Definition, inputTypes, outputType, body);
				}),
			$author$project$Morphir$ListOfResults$liftAllErrors(
				A2(
					$elm$core$List$map,
					function (_v0) {
						var name = _v0.a;
						var attr = _v0.b;
						var tpe = _v0.c;
						return A2(
							$elm$core$Result$map,
							function (t) {
								return _Utils_Tuple3(name, attr, t);
							},
							mapType(tpe));
					},
					def.a3)),
			A2(
				$elm$core$Result$mapError,
				$elm$core$List$singleton,
				mapType(def.fq)),
			A2(
				$elm$core$Result$mapError,
				$elm$core$List$singleton,
				mapValue(def.dV)));
	});
var $author$project$Morphir$Elm$Frontend$DuplicateNameInPattern = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $author$project$Morphir$Elm$Frontend$ResolveError = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Morphir$Elm$Frontend$VariableShadowing = F3(
	function (a, b, c) {
		return {$: 6, a: a, b: b, c: c};
	});
var $elm$core$Set$intersect = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$intersect, dict1, dict2);
	});
var $author$project$Morphir$Elm$Frontend$resolvePatternReferences = F2(
	function (moduleResolver, pattern) {
		_v0$4:
		while (true) {
			switch (pattern.$) {
				case 1:
					var sourceLocation = pattern.a;
					var subjectPattern = pattern.b;
					var alias = pattern.c;
					return A2(
						$elm$core$Result$map,
						function (resolvedSubjectPattern) {
							return A3($author$project$Morphir$IR$Value$AsPattern, sourceLocation, resolvedSubjectPattern, alias);
						},
						A2($author$project$Morphir$Elm$Frontend$resolvePatternReferences, moduleResolver, subjectPattern));
				case 2:
					var sourceLocation = pattern.a;
					var elems = pattern.b;
					return A2(
						$elm$core$Result$map,
						function (resolvedElems) {
							return A2($author$project$Morphir$IR$Value$TuplePattern, sourceLocation, resolvedElems);
						},
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									$author$project$Morphir$Elm$Frontend$resolvePatternReferences(moduleResolver),
									elems))));
				case 3:
					if (!pattern.b.a.b) {
						var sourceLocation = pattern.a;
						var _v1 = pattern.b;
						var modulePath = _v1.b;
						var localName = _v1.c;
						var argPatterns = pattern.c;
						return A3(
							$elm$core$Result$map2,
							F2(
								function (resolvedFullName, resolvedArgPatterns) {
									return A3($author$project$Morphir$IR$Value$ConstructorPattern, sourceLocation, resolvedFullName, resolvedArgPatterns);
								}),
							A2(
								$elm$core$Result$mapError,
								A2(
									$elm$core$Basics$composeR,
									$author$project$Morphir$Elm$Frontend$ResolveError(sourceLocation),
									$elm$core$List$singleton),
								A2(
									moduleResolver.c5,
									A2($elm$core$List$map, $author$project$Morphir$IR$Name$toTitleCase, modulePath),
									$author$project$Morphir$IR$Name$toTitleCase(localName))),
							A2(
								$elm$core$Result$mapError,
								$elm$core$List$concat,
								$author$project$Morphir$ListOfResults$liftAllErrors(
									A2(
										$elm$core$List$map,
										$author$project$Morphir$Elm$Frontend$resolvePatternReferences(moduleResolver),
										argPatterns))));
					} else {
						break _v0$4;
					}
				case 5:
					var sourceLocation = pattern.a;
					var headPattern = pattern.b;
					var tailPattern = pattern.c;
					return A3(
						$elm$core$Result$map2,
						$author$project$Morphir$IR$Value$HeadTailPattern(sourceLocation),
						A2($author$project$Morphir$Elm$Frontend$resolvePatternReferences, moduleResolver, headPattern),
						A2($author$project$Morphir$Elm$Frontend$resolvePatternReferences, moduleResolver, tailPattern));
				default:
					break _v0$4;
			}
		}
		return $elm$core$Result$Ok(pattern);
	});
var $author$project$Morphir$Rule$defaultToOriginal = F2(
	function (rule, a) {
		var _v0 = rule(a);
		if (_v0.$ === 1) {
			return $elm$core$Result$Ok(a);
		} else {
			var result = _v0.a;
			return result;
		}
	});
var $author$project$Morphir$Rewrite$bottomUp = F3(
	function (rewrite, rewriteRule, nodeToRewrite) {
		var top = A3(
			rewrite,
			function (a) {
				return A3($author$project$Morphir$Rewrite$bottomUp, rewrite, rewriteRule, a);
			},
			$author$project$Morphir$Rule$defaultToOriginal(rewriteRule),
			nodeToRewrite);
		var _v0 = A2($elm$core$Result$map, rewriteRule, top);
		if (!_v0.$) {
			if (_v0.a.$ === 1) {
				var _v1 = _v0.a;
				return top;
			} else {
				var result = _v0.a.a;
				return result;
			}
		} else {
			var error = _v0.a;
			return $elm$core$Result$Err(error);
		}
	});
var $author$project$Morphir$IR$Type$Rewrite$rewriteType = F3(
	function (rewriteBranch, rewriteLeaf, typeToRewrite) {
		switch (typeToRewrite.$) {
			case 1:
				var a = typeToRewrite.a;
				var fQName = typeToRewrite.b;
				var argTypes = typeToRewrite.c;
				return A2(
					$elm$core$Result$map,
					A2($author$project$Morphir$IR$Type$Reference, a, fQName),
					A3(
						$elm$core$List$foldr,
						F2(
							function (nextArg, resultSoFar) {
								return A3(
									$elm$core$Result$map2,
									$elm$core$List$cons,
									rewriteBranch(nextArg),
									resultSoFar);
							}),
						$elm$core$Result$Ok(_List_Nil),
						argTypes));
			case 2:
				var a = typeToRewrite.a;
				var elemTypes = typeToRewrite.b;
				return A2(
					$elm$core$Result$map,
					$author$project$Morphir$IR$Type$Tuple(a),
					A3(
						$elm$core$List$foldr,
						F2(
							function (nextArg, resultSoFar) {
								return A3(
									$elm$core$Result$map2,
									$elm$core$List$cons,
									rewriteBranch(nextArg),
									resultSoFar);
							}),
						$elm$core$Result$Ok(_List_Nil),
						elemTypes));
			case 3:
				var a = typeToRewrite.a;
				var fieldTypes = typeToRewrite.b;
				return A2(
					$elm$core$Result$map,
					$author$project$Morphir$IR$Type$Record(a),
					A3(
						$elm$core$List$foldr,
						F2(
							function (field, resultSoFar) {
								return A3(
									$elm$core$Result$map2,
									$elm$core$List$cons,
									A2(
										$elm$core$Result$map,
										$author$project$Morphir$IR$Type$Field(field.cN),
										rewriteBranch(field.gl)),
									resultSoFar);
							}),
						$elm$core$Result$Ok(_List_Nil),
						fieldTypes));
			case 4:
				var a = typeToRewrite.a;
				var varName = typeToRewrite.b;
				var fieldTypes = typeToRewrite.c;
				return A2(
					$elm$core$Result$map,
					A2($author$project$Morphir$IR$Type$ExtensibleRecord, a, varName),
					A3(
						$elm$core$List$foldr,
						F2(
							function (field, resultSoFar) {
								return A3(
									$elm$core$Result$map2,
									$elm$core$List$cons,
									A2(
										$elm$core$Result$map,
										$author$project$Morphir$IR$Type$Field(field.cN),
										rewriteBranch(field.gl)),
									resultSoFar);
							}),
						$elm$core$Result$Ok(_List_Nil),
						fieldTypes));
			case 5:
				var a = typeToRewrite.a;
				var argType = typeToRewrite.b;
				var returnType = typeToRewrite.c;
				return A3(
					$elm$core$Result$map2,
					$author$project$Morphir$IR$Type$Function(a),
					rewriteBranch(argType),
					rewriteBranch(returnType));
			default:
				return rewriteLeaf(typeToRewrite);
		}
	});
var $author$project$Morphir$Elm$Frontend$rewriteTypes = function (moduleResolver) {
	return A2(
		$author$project$Morphir$Rewrite$bottomUp,
		$author$project$Morphir$IR$Type$Rewrite$rewriteType,
		function (tpe) {
			if (tpe.$ === 1) {
				var sourceLocation = tpe.a;
				var refFullName = tpe.b;
				var args = tpe.c;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$mapError,
						A2(
							$elm$core$Basics$composeR,
							$author$project$Morphir$Elm$Frontend$ResolveError(sourceLocation),
							$elm$core$List$singleton),
						A2(
							$elm$core$Result$map,
							function (resolvedFullName) {
								return A3($author$project$Morphir$IR$Type$Reference, sourceLocation, resolvedFullName, args);
							},
							A2(
								moduleResolver.fz,
								A2(
									$elm$core$List$map,
									$author$project$Morphir$IR$Name$toTitleCase,
									$author$project$Morphir$IR$FQName$getModulePath(refFullName)),
								$author$project$Morphir$IR$Name$toTitleCase(
									$author$project$Morphir$IR$FQName$getLocalName(refFullName))))));
			} else {
				return $elm$core$Maybe$Nothing;
			}
		});
};
var $author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences = F3(
	function (variables, moduleResolver, value) {
		var unionNames = F3(
			function (toError, namesA, namesB) {
				var duplicateNames = $elm$core$Set$toList(
					A2(
						$elm$core$Set$intersect,
						$elm$core$Set$fromList(
							$elm$core$Dict$keys(namesA)),
						$elm$core$Set$fromList(
							$elm$core$Dict$keys(namesB))));
				return $elm$core$List$isEmpty(duplicateNames) ? $elm$core$Result$Ok(
					A2($elm$core$Dict$union, namesA, namesB)) : $elm$core$Result$Err(
					A2(
						$elm$core$List$filterMap,
						function (name) {
							return A3(
								$elm$core$Maybe$map2,
								toError(name),
								A2($elm$core$Dict$get, name, namesA),
								A2($elm$core$Dict$get, name, namesB));
						},
						duplicateNames));
			});
		var unionPatternNames = unionNames($author$project$Morphir$Elm$Frontend$DuplicateNameInPattern);
		var unionVariableNames = unionNames($author$project$Morphir$Elm$Frontend$VariableShadowing);
		var resolveValueDefinition = F2(
			function (def, variablesDefNamesAndArgs) {
				return A4(
					$elm$core$Result$map3,
					$author$project$Morphir$IR$Value$Definition,
					A2(
						$elm$core$Result$mapError,
						$elm$core$List$concat,
						$author$project$Morphir$ListOfResults$liftAllErrors(
							A2(
								$elm$core$List$map,
								function (_v12) {
									var argName = _v12.a;
									var a = _v12.b;
									var argType = _v12.c;
									return A2(
										$elm$core$Result$map,
										function (t) {
											return _Utils_Tuple3(argName, a, t);
										},
										A2($author$project$Morphir$Elm$Frontend$rewriteTypes, moduleResolver, argType));
								},
								def.a3))),
					A2($author$project$Morphir$Elm$Frontend$rewriteTypes, moduleResolver, def.fq),
					A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variablesDefNamesAndArgs, moduleResolver, def.dV));
			});
		var namesBoundInPattern = function (pattern) {
			switch (pattern.$) {
				case 1:
					var sourceLocation = pattern.a;
					var subjectPattern = pattern.b;
					var alias = pattern.c;
					return A2(
						$elm$core$Result$andThen,
						function (subjectNames) {
							return A2(
								unionPatternNames,
								subjectNames,
								A2($elm$core$Dict$singleton, alias, sourceLocation));
						},
						namesBoundInPattern(subjectPattern));
				case 2:
					var elems = pattern.b;
					return A3(
						$elm$core$List$foldl,
						F2(
							function (nextNames, soFar) {
								return A2(
									$elm$core$Result$andThen,
									function (namesSoFar) {
										return A2(
											$elm$core$Result$andThen,
											unionPatternNames(namesSoFar),
											nextNames);
									},
									soFar);
							}),
						$elm$core$Result$Ok($elm$core$Dict$empty),
						A2($elm$core$List$map, namesBoundInPattern, elems));
				case 3:
					var args = pattern.c;
					return A3(
						$elm$core$List$foldl,
						F2(
							function (nextNames, soFar) {
								return A2(
									$elm$core$Result$andThen,
									function (namesSoFar) {
										return A2(
											$elm$core$Result$andThen,
											unionPatternNames(namesSoFar),
											nextNames);
									},
									soFar);
							}),
						$elm$core$Result$Ok($elm$core$Dict$empty),
						A2($elm$core$List$map, namesBoundInPattern, args));
				case 5:
					var headPattern = pattern.b;
					var tailPattern = pattern.c;
					return A2(
						$elm$core$Result$andThen,
						function (headNames) {
							return A2(
								$elm$core$Result$andThen,
								unionPatternNames(headNames),
								namesBoundInPattern(tailPattern));
						},
						namesBoundInPattern(headPattern));
				default:
					return $elm$core$Result$Ok($elm$core$Dict$empty);
			}
		};
		_v1$14:
		while (true) {
			switch (value.$) {
				case 1:
					if (!value.b.a.b) {
						var sourceLocation = value.a;
						var _v2 = value.b;
						var modulePath = _v2.b;
						var localName = _v2.c;
						return A2(
							$elm$core$Result$mapError,
							A2(
								$elm$core$Basics$composeR,
								$author$project$Morphir$Elm$Frontend$ResolveError(sourceLocation),
								$elm$core$List$singleton),
							A2(
								$elm$core$Result$map,
								function (resolvedFullName) {
									return A2($author$project$Morphir$IR$Value$Constructor, sourceLocation, resolvedFullName);
								},
								A2(
									moduleResolver.c5,
									A2($elm$core$List$map, $author$project$Morphir$IR$Name$toTitleCase, modulePath),
									$author$project$Morphir$IR$Name$toTitleCase(localName))));
					} else {
						break _v1$14;
					}
				case 6:
					if (!value.b.a.b) {
						var sourceLocation = value.a;
						var _v3 = value.b;
						var modulePath = _v3.b;
						var localName = _v3.c;
						return A2($elm$core$Dict$member, localName, variables) ? $elm$core$Result$Ok(
							A2($author$project$Morphir$IR$Value$Variable, sourceLocation, localName)) : A2(
							$elm$core$Result$mapError,
							A2(
								$elm$core$Basics$composeR,
								$author$project$Morphir$Elm$Frontend$ResolveError(sourceLocation),
								$elm$core$List$singleton),
							A2(
								$elm$core$Result$map,
								function (resolvedFullName) {
									return A2($author$project$Morphir$IR$Value$Reference, sourceLocation, resolvedFullName);
								},
								A2(
									moduleResolver.fA,
									A2($elm$core$List$map, $author$project$Morphir$IR$Name$toTitleCase, modulePath),
									$author$project$Morphir$IR$Name$toCamelCase(localName))));
					} else {
						break _v1$14;
					}
				case 10:
					var a = value.a;
					var argPattern = value.b;
					var bodyValue = value.c;
					return A3(
						$elm$core$Result$map2,
						$author$project$Morphir$IR$Value$Lambda(a),
						A2($author$project$Morphir$Elm$Frontend$resolvePatternReferences, moduleResolver, argPattern),
						A2(
							$elm$core$Result$andThen,
							function (newVariables) {
								return A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, newVariables, moduleResolver, bodyValue);
							},
							A2(
								$elm$core$Result$andThen,
								function (patternNames) {
									return A2(unionVariableNames, variables, patternNames);
								},
								namesBoundInPattern(argPattern))));
				case 11:
					var sourceLocation = value.a;
					var name = value.b;
					var def = value.c;
					var inValue = value.d;
					return A3(
						$elm$core$Result$map2,
						A2($author$project$Morphir$IR$Value$LetDefinition, sourceLocation, name),
						A2(
							$elm$core$Result$andThen,
							resolveValueDefinition(def),
							A2(
								unionVariableNames,
								variables,
								A3(
									$elm$core$Dict$insert,
									name,
									sourceLocation,
									$elm$core$Dict$fromList(
										A2(
											$elm$core$List$map,
											function (_v4) {
												var argName = _v4.a;
												var loc = _v4.b;
												return _Utils_Tuple2(argName, loc);
											},
											def.a3))))),
						A2(
							$elm$core$Result$andThen,
							function (newVariables) {
								return A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, newVariables, moduleResolver, inValue);
							},
							A2(
								unionVariableNames,
								variables,
								A2($elm$core$Dict$singleton, name, sourceLocation))));
				case 12:
					var sourceLocation = value.a;
					var defs = value.b;
					var inValue = value.c;
					return A2(
						$elm$core$Result$andThen,
						function (variablesAndDefNames) {
							return A3(
								$elm$core$Result$map2,
								$author$project$Morphir$IR$Value$LetRecursion(sourceLocation),
								A2(
									$elm$core$Result$map,
									$elm$core$Dict$fromList,
									A2(
										$elm$core$Result$mapError,
										$elm$core$List$concat,
										$author$project$Morphir$ListOfResults$liftAllErrors(
											A2(
												$elm$core$List$map,
												function (_v7) {
													var name = _v7.a;
													var def = _v7.b;
													return A2(
														$elm$core$Result$andThen,
														function (variablesDefNamesAndArgs) {
															return A2(
																$elm$core$Result$map,
																$elm$core$Tuple$pair(name),
																A2(resolveValueDefinition, def, variablesDefNamesAndArgs));
														},
														A2(
															unionVariableNames,
															variablesAndDefNames,
															$elm$core$Dict$fromList(
																A2(
																	$elm$core$List$map,
																	function (_v8) {
																		var argName = _v8.a;
																		var loc = _v8.b;
																		return _Utils_Tuple2(argName, loc);
																	},
																	def.a3))));
												},
												$elm$core$Dict$toList(defs))))),
								A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variablesAndDefNames, moduleResolver, inValue));
						},
						A2(
							unionVariableNames,
							variables,
							A2(
								$elm$core$Dict$map,
								F2(
									function (_v5, _v6) {
										return sourceLocation;
									}),
								defs)));
				case 13:
					var a = value.a;
					var pattern = value.b;
					var subjectValue = value.c;
					var inValue = value.d;
					return A4(
						$elm$core$Result$map3,
						$author$project$Morphir$IR$Value$Destructure(a),
						A2($author$project$Morphir$Elm$Frontend$resolvePatternReferences, moduleResolver, pattern),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, subjectValue),
						A2(
							$elm$core$Result$andThen,
							function (newVariables) {
								return A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, newVariables, moduleResolver, inValue);
							},
							A2(
								$elm$core$Result$andThen,
								function (patternNames) {
									return A2(unionVariableNames, variables, patternNames);
								},
								namesBoundInPattern(pattern))));
				case 15:
					var a = value.a;
					var matchValue = value.b;
					var cases = value.c;
					return A3(
						$elm$core$Result$map2,
						$author$project$Morphir$IR$Value$PatternMatch(a),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, matchValue),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									function (_v9) {
										var casePattern = _v9.a;
										var caseValue = _v9.b;
										return A3(
											$elm$core$Result$map2,
											$elm$core$Tuple$pair,
											A2($author$project$Morphir$Elm$Frontend$resolvePatternReferences, moduleResolver, casePattern),
											A2(
												$elm$core$Result$andThen,
												function (newVariables) {
													return A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, newVariables, moduleResolver, caseValue);
												},
												A2(
													$elm$core$Result$andThen,
													function (patternNames) {
														return A2(unionVariableNames, variables, patternNames);
													},
													namesBoundInPattern(casePattern))));
									},
									cases))));
				case 2:
					var a = value.a;
					var elems = value.b;
					return A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Value$Tuple(a),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									A2($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver),
									elems))));
				case 3:
					var a = value.a;
					var items = value.b;
					return A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Value$List(a),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									A2($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver),
									items))));
				case 4:
					var a = value.a;
					var fields = value.b;
					return A2(
						$elm$core$Result$map,
						$author$project$Morphir$IR$Value$Record(a),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									function (_v10) {
										var fieldName = _v10.a;
										var fieldValue = _v10.b;
										return A2(
											$elm$core$Result$map,
											$elm$core$Tuple$pair(fieldName),
											A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, fieldValue));
									},
									fields))));
				case 7:
					var a = value.a;
					var subjectValue = value.b;
					var fieldName = value.c;
					return A2(
						$elm$core$Result$map,
						function (s) {
							return A3($author$project$Morphir$IR$Value$Field, a, s, fieldName);
						},
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, subjectValue));
				case 9:
					var a = value.a;
					var funValue = value.b;
					var argValue = value.c;
					return A3(
						$elm$core$Result$map2,
						$author$project$Morphir$IR$Value$Apply(a),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, funValue),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, argValue));
				case 14:
					var a = value.a;
					var condValue = value.b;
					var thenValue = value.c;
					var elseValue = value.d;
					return A4(
						$elm$core$Result$map3,
						$author$project$Morphir$IR$Value$IfThenElse(a),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, condValue),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, thenValue),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, elseValue));
				case 16:
					var a = value.a;
					var subjectValue = value.b;
					var newFieldValues = value.c;
					return A3(
						$elm$core$Result$map2,
						$author$project$Morphir$IR$Value$UpdateRecord(a),
						A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, subjectValue),
						A2(
							$elm$core$Result$mapError,
							$elm$core$List$concat,
							$author$project$Morphir$ListOfResults$liftAllErrors(
								A2(
									$elm$core$List$map,
									function (_v11) {
										var fieldName = _v11.a;
										var fieldValue = _v11.b;
										return A2(
											$elm$core$Result$map,
											$elm$core$Tuple$pair(fieldName),
											A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, fieldValue));
									},
									newFieldValues))));
				default:
					break _v1$14;
			}
		}
		return $elm$core$Result$Ok(value);
	});
var $author$project$Morphir$Elm$Frontend$resolveLocalNames = F2(
	function (moduleResolver, moduleDef) {
		var typesResult = A2(
			$elm$core$Result$mapError,
			$elm$core$List$concat,
			A2(
				$elm$core$Result$map,
				$elm$core$Dict$fromList,
				$author$project$Morphir$ListOfResults$liftAllErrors(
					A2(
						$elm$core$List$map,
						function (_v2) {
							var typeName = _v2.a;
							var typeDef = _v2.b;
							return A2(
								$elm$core$Result$mapError,
								$elm$core$List$concat,
								A2(
									$elm$core$Result$map,
									$elm$core$Tuple$pair(typeName),
									A2(
										$elm$core$Result$map,
										$author$project$Morphir$IR$AccessControlled$AccessControlled(typeDef.b0),
										A2(
											$elm$core$Result$map,
											$author$project$Morphir$IR$Documented$Documented(typeDef.aT.en),
											A2(
												$author$project$Morphir$IR$Type$mapDefinition,
												$author$project$Morphir$Elm$Frontend$rewriteTypes(moduleResolver),
												typeDef.aT.aT)))));
						},
						$elm$core$Dict$toList(moduleDef.gn)))));
		var rewriteValues = F2(
			function (variables, value) {
				return A3($author$project$Morphir$Elm$Frontend$resolveVariablesAndReferences, variables, moduleResolver, value);
			});
		var valuesResult = A2(
			$elm$core$Result$mapError,
			$elm$core$List$concat,
			A2(
				$elm$core$Result$map,
				$elm$core$Dict$fromList,
				$author$project$Morphir$ListOfResults$liftAllErrors(
					A2(
						$elm$core$List$map,
						function (_v0) {
							var valueName = _v0.a;
							var valueDef = _v0.b;
							var variables = $elm$core$Dict$fromList(
								A2(
									$elm$core$List$map,
									function (_v1) {
										var name = _v1.a;
										var loc = _v1.b;
										return _Utils_Tuple2(name, loc);
									},
									valueDef.aT.a3));
							return A2(
								$elm$core$Result$mapError,
								$elm$core$List$concat,
								A2(
									$elm$core$Result$map,
									$elm$core$Tuple$pair(valueName),
									A2(
										$elm$core$Result$map,
										$author$project$Morphir$IR$AccessControlled$AccessControlled(valueDef.b0),
										A3(
											$author$project$Morphir$IR$Value$mapDefinition,
											$author$project$Morphir$Elm$Frontend$rewriteTypes(moduleResolver),
											rewriteValues(variables),
											valueDef.aT))));
						},
						$elm$core$Dict$toList(moduleDef.gq)))));
		return A3($elm$core$Result$map2, $author$project$Morphir$IR$Module$Definition, typesResult, valuesResult);
	});
var $author$project$Morphir$Elm$Frontend$mapProcessedFile = F4(
	function (dependencies, currentPackagePath, processedFile, modulesSoFar) {
		var modulePath = A2(
			$elm$core$List$drop,
			$elm$core$List$length(currentPackagePath),
			$author$project$Morphir$IR$Path$fromList(
				A2(
					$elm$core$List$map,
					$author$project$Morphir$IR$Name$fromString,
					$stil4m$elm_syntax$Elm$Syntax$Module$moduleName(
						$stil4m$elm_syntax$Elm$Syntax$Node$value(processedFile.az.cL)))));
		var moduleExpose = $stil4m$elm_syntax$Elm$Syntax$Module$exposingList(
			$stil4m$elm_syntax$Elm$Syntax$Node$value(processedFile.az.cL));
		var typesResult = A2(
			$elm$core$Result$map,
			$elm$core$Dict$fromList,
			A3(
				$author$project$Morphir$Elm$Frontend$mapDeclarationsToType,
				processedFile.bO.bT,
				moduleExpose,
				A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, processedFile.az.a0)));
		var valuesResult = A2(
			$elm$core$Result$map,
			$elm$core$Dict$fromList,
			A3($author$project$Morphir$Elm$Frontend$mapDeclarationsToValue, processedFile.bO.bT, moduleExpose, processedFile.az.a0));
		var moduleResult = A3($elm$core$Result$map2, $author$project$Morphir$IR$Module$Definition, typesResult, valuesResult);
		var moduleDeclsSoFar = A2(
			$elm$core$Dict$map,
			F2(
				function (_v0, def) {
					return $author$project$Morphir$IR$Module$eraseSpecificationAttributes(
						$author$project$Morphir$IR$Module$definitionToSpecification(def));
				}),
			modulesSoFar);
		return A2(
			$elm$core$Result$map,
			function (m) {
				return A3($elm$core$Dict$insert, modulePath, m, modulesSoFar);
			},
			A2(
				$elm$core$Result$andThen,
				function (moduleDef) {
					var moduleResolver = $author$project$Morphir$Elm$Frontend$Resolve$createModuleResolver(
						A6(
							$author$project$Morphir$Elm$Frontend$Resolve$Context,
							A2($elm$core$Dict$union, $author$project$Morphir$Elm$Frontend$defaultDependencies, dependencies),
							currentPackagePath,
							moduleDeclsSoFar,
							A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, processedFile.az.eM),
							modulePath,
							moduleDef));
					return A2($author$project$Morphir$Elm$Frontend$resolveLocalNames, moduleResolver, moduleDef);
				},
				moduleResult));
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$Application = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Processing$expressionOperators = function (_v0) {
	var expression = _v0.b;
	if (expression.$ === 6) {
		var s = expression.a;
		return $elm$core$Maybe$Just(s);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$list_extra$List$Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				if (!list.b) {
					return $elm$core$List$reverse(memo);
				} else {
					var x = list.a;
					var xs = list.b;
					if (predicate(x)) {
						var $temp$memo = A2($elm$core$List$cons, x, memo),
							$temp$list = xs;
						memo = $temp$memo;
						list = $temp$list;
						continue takeWhileMemo;
					} else {
						return $elm$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(_List_Nil);
};
var $stil4m$elm_syntax$Elm$Processing$findNextSplit = F2(
	function (dict, exps) {
		var prefix = A2(
			$elm_community$list_extra$List$Extra$takeWhile,
			function (x) {
				return _Utils_eq(
					$elm$core$Maybe$Nothing,
					A2(
						$elm$core$Maybe$andThen,
						function (key) {
							return A2($elm$core$Dict$get, key, dict);
						},
						$stil4m$elm_syntax$Elm$Processing$expressionOperators(x)));
			},
			exps);
		var suffix = A2(
			$elm$core$List$drop,
			$elm$core$List$length(prefix) + 1,
			exps);
		return A2(
			$elm$core$Maybe$map,
			function (x) {
				return _Utils_Tuple3(prefix, x, suffix);
			},
			A2(
				$elm$core$Maybe$andThen,
				function (x) {
					return A2($elm$core$Dict$get, x, dict);
				},
				A2(
					$elm$core$Maybe$andThen,
					$stil4m$elm_syntax$Elm$Processing$expressionOperators,
					$elm$core$List$head(
						A2(
							$elm$core$List$drop,
							$elm$core$List$length(prefix),
							exps)))));
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $stil4m$elm_syntax$Elm$Processing$lowestPrecedence = function (input) {
	return $elm$core$Dict$fromList(
		A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				function (m) {
					return A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$second,
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.q;
								},
								A2(
									$elm$core$Basics$composeR,
									$stil4m$elm_syntax$Elm$Syntax$Node$value,
									$elm$core$Basics$eq(m)))),
						input);
				},
				$elm$core$List$minimum(
					A2(
						$elm$core$List$map,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$second,
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.q;
								},
								$stil4m$elm_syntax$Elm$Syntax$Node$value)),
						input)))));
};
var $stil4m$elm_syntax$Elm$Syntax$Node$range = function (_v0) {
	var r = _v0.a;
	return r;
};
var $stil4m$elm_syntax$Elm$Processing$fixApplication = F2(
	function (operators, expressions) {
		var ops = $stil4m$elm_syntax$Elm$Processing$lowestPrecedence(
			A2(
				$elm$core$List$map,
				function (x) {
					return _Utils_Tuple2(
						x,
						A2(
							$elm$core$Maybe$withDefault,
							{
								m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
								n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'todo'),
								o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, x),
								q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 5)
							},
							A2($elm$core$Dict$get, x, operators)));
				},
				A2($elm$core$List$filterMap, $stil4m$elm_syntax$Elm$Processing$expressionOperators, expressions)));
		var fixExprs = function (exps) {
			if (exps.b && (!exps.b.b)) {
				var _v2 = exps.a;
				var x = _v2.b;
				return x;
			} else {
				return $stil4m$elm_syntax$Elm$Syntax$Expression$Application(exps);
			}
		};
		var divideAndConquer = function (exps) {
			return $elm$core$Dict$isEmpty(ops) ? fixExprs(exps) : A2(
				$elm$core$Maybe$withDefault,
				fixExprs(exps),
				A2(
					$elm$core$Maybe$map,
					function (_v0) {
						var p = _v0.a;
						var infix = _v0.b;
						var s = _v0.c;
						return A4(
							$stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication,
							$stil4m$elm_syntax$Elm$Syntax$Node$value(infix.o),
							$stil4m$elm_syntax$Elm$Syntax$Node$value(infix.m),
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								$stil4m$elm_syntax$Elm$Syntax$Range$combine(
									A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$range, p)),
								divideAndConquer(p)),
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								$stil4m$elm_syntax$Elm$Syntax$Range$combine(
									A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$range, s)),
								divideAndConquer(s)));
					},
					A2($stil4m$elm_syntax$Elm$Processing$findNextSplit, ops, exps)));
		};
		return divideAndConquer(expressions);
	});
var $stil4m$elm_syntax$Elm$Inspector$Post = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Inspector$Continue = {$: 1};
var $stil4m$elm_syntax$Elm$Inspector$defaultConfig = {bv: $stil4m$elm_syntax$Elm$Inspector$Continue, bw: $stil4m$elm_syntax$Elm$Inspector$Continue, bx: $stil4m$elm_syntax$Elm$Inspector$Continue, by: $stil4m$elm_syntax$Elm$Inspector$Continue, bz: $stil4m$elm_syntax$Elm$Inspector$Continue, bA: $stil4m$elm_syntax$Elm$Inspector$Continue, bB: $stil4m$elm_syntax$Elm$Inspector$Continue, bC: $stil4m$elm_syntax$Elm$Inspector$Continue, bD: $stil4m$elm_syntax$Elm$Inspector$Continue, bE: $stil4m$elm_syntax$Elm$Inspector$Continue, bF: $stil4m$elm_syntax$Elm$Inspector$Continue, bG: $stil4m$elm_syntax$Elm$Inspector$Continue, bH: $stil4m$elm_syntax$Elm$Inspector$Continue, bI: $stil4m$elm_syntax$Elm$Inspector$Continue, bJ: $stil4m$elm_syntax$Elm$Inspector$Continue, bK: $stil4m$elm_syntax$Elm$Inspector$Continue, bL: $stil4m$elm_syntax$Elm$Inspector$Continue, bM: $stil4m$elm_syntax$Elm$Inspector$Continue};
var $stil4m$elm_syntax$Elm$Inspector$actionLambda = function (act) {
	switch (act.$) {
		case 0:
			return F3(
				function (_v1, _v2, c) {
					return c;
				});
		case 1:
			return F3(
				function (f, _v3, c) {
					return f(c);
				});
		case 2:
			var g = act.a;
			return F3(
				function (f, x, c) {
					return f(
						A2(g, x, c));
				});
		case 3:
			var g = act.a;
			return F3(
				function (f, x, c) {
					return A2(
						g,
						x,
						f(c));
				});
		default:
			var g = act.a;
			return F3(
				function (f, x, c) {
					return A3(g, f, x, c);
				});
	}
};
var $stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation = F3(
	function (config, typeAnnotation, context) {
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bM,
			A2($stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotationInner, config, typeAnnotation),
			typeAnnotation,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotationInner = F3(
	function (config, _v0, context) {
		var typeRefence = _v0.b;
		switch (typeRefence.$) {
			case 1:
				var typeArgs = typeRefence.b;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation(config),
					context,
					typeArgs);
			case 3:
				var typeAnnotations = typeRefence.a;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation(config),
					context,
					typeAnnotations);
			case 4:
				var recordDefinition = typeRefence.a;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation(config),
					context,
					A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, $elm$core$Tuple$second),
						recordDefinition));
			case 5:
				var recordDefinition = typeRefence.b;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation(config),
					context,
					A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Syntax$Node$value, $elm$core$Tuple$second),
						$stil4m$elm_syntax$Elm$Syntax$Node$value(recordDefinition)));
			case 6:
				var left = typeRefence.a;
				var right = typeRefence.b;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation(config),
					context,
					_List_fromArray(
						[left, right]));
			case 2:
				return context;
			default:
				return context;
		}
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectSignature = F3(
	function (config, node, context) {
		var signature = node.b;
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bJ,
			A2($stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation, config, signature.dn),
			node,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectCase = F3(
	function (config, caze, context) {
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bv,
			A2($stil4m$elm_syntax$Elm$Inspector$inspectExpression, config, caze.b),
			caze,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectDestructuring = F3(
	function (config, destructuring, context) {
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bw,
			function (c) {
				return A3(
					$stil4m$elm_syntax$Elm$Inspector$inspectExpression,
					config,
					$stil4m$elm_syntax$Elm$Syntax$Node$value(destructuring).b,
					c);
			},
			destructuring,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectExpression = F3(
	function (config, node, context) {
		var expression = node.b;
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bx,
			A2($stil4m$elm_syntax$Elm$Inspector$inspectInnerExpression, config, expression),
			node,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectFunction = F3(
	function (config, node, context) {
		var _function = node.b;
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bz,
			A2(
				$elm$core$Basics$composeR,
				A2(
					$stil4m$elm_syntax$Elm$Inspector$inspectExpression,
					config,
					$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am)._),
				A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Basics$identity,
					A2(
						$elm$core$Maybe$map,
						$stil4m$elm_syntax$Elm$Inspector$inspectSignature(config),
						_function.fL))),
			node,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectInnerExpression = F3(
	function (config, expression, context) {
		switch (expression.$) {
			case 0:
				return context;
			case 3:
				var moduleName = expression.a;
				var functionOrVal = expression.b;
				return A4(
					$stil4m$elm_syntax$Elm$Inspector$actionLambda,
					config.bA,
					$elm$core$Basics$identity,
					_Utils_Tuple2(moduleName, functionOrVal),
					context);
			case 5:
				return context;
			case 6:
				return context;
			case 8:
				return context;
			case 7:
				return context;
			case 9:
				return context;
			case 10:
				var x = expression.a;
				return A3($stil4m$elm_syntax$Elm$Inspector$inspectExpression, config, x, context);
			case 11:
				return context;
			case 12:
				return context;
			case 20:
				var ex1 = expression.a;
				var key = expression.b;
				return A4(
					$stil4m$elm_syntax$Elm$Inspector$actionLambda,
					config.bH,
					A2($stil4m$elm_syntax$Elm$Inspector$inspectExpression, config, ex1),
					_Utils_Tuple2(ex1, key),
					context);
			case 21:
				return context;
			case 23:
				return context;
			case 1:
				var expressionList = expression.a;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectExpression(config),
					context,
					expressionList);
			case 2:
				var op = expression.a;
				var dir = expression.b;
				var left = expression.c;
				var right = expression.d;
				return A4(
					$stil4m$elm_syntax$Elm$Inspector$actionLambda,
					config.bF,
					function (base) {
						return A3(
							$elm$core$List$foldl,
							$stil4m$elm_syntax$Elm$Inspector$inspectExpression(config),
							base,
							_List_fromArray(
								[left, right]));
					},
					{m: dir, c: left, o: op, d: right},
					context);
			case 4:
				var e1 = expression.a;
				var e2 = expression.b;
				var e3 = expression.c;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectExpression(config),
					context,
					_List_fromArray(
						[e1, e2, e3]));
			case 13:
				var expressionList = expression.a;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectExpression(config),
					context,
					expressionList);
			case 14:
				var inner = expression.a;
				return A3($stil4m$elm_syntax$Elm$Inspector$inspectExpression, config, inner, context);
			case 15:
				var letBlock = expression.a;
				var next = A2(
					$elm$core$Basics$composeR,
					A2($stil4m$elm_syntax$Elm$Inspector$inspectLetDeclarations, config, letBlock.a0),
					A2($stil4m$elm_syntax$Elm$Inspector$inspectExpression, config, letBlock._));
				return A4($stil4m$elm_syntax$Elm$Inspector$actionLambda, config.bE, next, letBlock, context);
			case 16:
				var caseBlock = expression.a;
				var context2 = A3($stil4m$elm_syntax$Elm$Inspector$inspectExpression, config, caseBlock._, context);
				var context3 = A3(
					$elm$core$List$foldl,
					F2(
						function (a, b) {
							return A3($stil4m$elm_syntax$Elm$Inspector$inspectCase, config, a, b);
						}),
					context2,
					caseBlock.b8);
				return context3;
			case 17:
				var lambda = expression.a;
				return A4(
					$stil4m$elm_syntax$Elm$Inspector$actionLambda,
					config.bD,
					A2($stil4m$elm_syntax$Elm$Inspector$inspectExpression, config, lambda._),
					lambda,
					context);
			case 19:
				var expressionList = expression.a;
				return A3(
					$elm$core$List$foldl,
					$stil4m$elm_syntax$Elm$Inspector$inspectExpression(config),
					context,
					expressionList);
			case 18:
				var expressionStringList = expression.a;
				return A3(
					$elm$core$List$foldl,
					F2(
						function (a, b) {
							return A3(
								$stil4m$elm_syntax$Elm$Inspector$inspectExpression,
								config,
								$stil4m$elm_syntax$Elm$Syntax$Node$value(a).b,
								b);
						}),
					context,
					expressionStringList);
			default:
				var name = expression.a;
				var updates = expression.b;
				return A4(
					$stil4m$elm_syntax$Elm$Inspector$actionLambda,
					config.bI,
					function (c) {
						return A3(
							$elm$core$List$foldl,
							F2(
								function (a, b) {
									return A3(
										$stil4m$elm_syntax$Elm$Inspector$inspectExpression,
										config,
										$stil4m$elm_syntax$Elm$Syntax$Node$value(a).b,
										b);
								}),
							c,
							updates);
					},
					_Utils_Tuple2(name, updates),
					context);
		}
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectLetDeclaration = F3(
	function (config, _v0, context) {
		var range = _v0.a;
		var declaration = _v0.b;
		if (!declaration.$) {
			var _function = declaration.a;
			return A3(
				$stil4m$elm_syntax$Elm$Inspector$inspectFunction,
				config,
				A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, _function),
				context);
		} else {
			var pattern = declaration.a;
			var expression = declaration.b;
			return A3(
				$stil4m$elm_syntax$Elm$Inspector$inspectDestructuring,
				config,
				A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					_Utils_Tuple2(pattern, expression)),
				context);
		}
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectLetDeclarations = F3(
	function (config, declarations, context) {
		return A3(
			$elm$core$List$foldl,
			$stil4m$elm_syntax$Elm$Inspector$inspectLetDeclaration(config),
			context,
			declarations);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectPortDeclaration = F3(
	function (config, signature, context) {
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bG,
			A2($stil4m$elm_syntax$Elm$Inspector$inspectSignature, config, signature),
			signature,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectValueConstructor = F3(
	function (config, _v0, context) {
		var valueConstructor = _v0.b;
		return A3(
			$elm$core$List$foldl,
			$stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation(config),
			context,
			valueConstructor.b4);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectTypeInner = F3(
	function (config, typeDecl, context) {
		return A3(
			$elm$core$List$foldl,
			$stil4m$elm_syntax$Elm$Inspector$inspectValueConstructor(config),
			context,
			typeDecl.ed);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectType = F3(
	function (config, tipe, context) {
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bK,
			A2(
				$stil4m$elm_syntax$Elm$Inspector$inspectTypeInner,
				config,
				$stil4m$elm_syntax$Elm$Syntax$Node$value(tipe)),
			tipe,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectTypeAlias = F3(
	function (config, pair, context) {
		var typeAlias = pair.b;
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.bL,
			A2($stil4m$elm_syntax$Elm$Inspector$inspectTypeAnnotation, config, typeAlias.dn),
			pair,
			context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectDeclaration = F3(
	function (config, _v0, context) {
		var r = _v0.a;
		var declaration = _v0.b;
		switch (declaration.$) {
			case 0:
				var _function = declaration.a;
				return A3(
					$stil4m$elm_syntax$Elm$Inspector$inspectFunction,
					config,
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, _function),
					context);
			case 1:
				var typeAlias = declaration.a;
				return A3(
					$stil4m$elm_syntax$Elm$Inspector$inspectTypeAlias,
					config,
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, typeAlias),
					context);
			case 2:
				var typeDecl = declaration.a;
				return A3(
					$stil4m$elm_syntax$Elm$Inspector$inspectType,
					config,
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, typeDecl),
					context);
			case 3:
				var signature = declaration.a;
				return A3(
					$stil4m$elm_syntax$Elm$Inspector$inspectPortDeclaration,
					config,
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, signature),
					context);
			case 4:
				var inf = declaration.a;
				return A4(
					$stil4m$elm_syntax$Elm$Inspector$actionLambda,
					config.bC,
					$elm$core$Basics$identity,
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, inf),
					context);
			default:
				var pattern = declaration.a;
				var expresion = declaration.b;
				return A3(
					$stil4m$elm_syntax$Elm$Inspector$inspectDestructuring,
					config,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						r,
						_Utils_Tuple2(pattern, expresion)),
					context);
		}
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectDeclarations = F3(
	function (config, declarations, context) {
		return A3(
			$elm$core$List$foldl,
			$stil4m$elm_syntax$Elm$Inspector$inspectDeclaration(config),
			context,
			declarations);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectImport = F3(
	function (config, imp, context) {
		return A4($stil4m$elm_syntax$Elm$Inspector$actionLambda, config.bB, $elm$core$Basics$identity, imp, context);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspectImports = F3(
	function (config, imports, context) {
		return A3(
			$elm$core$List$foldl,
			$stil4m$elm_syntax$Elm$Inspector$inspectImport(config),
			context,
			imports);
	});
var $stil4m$elm_syntax$Elm$Inspector$inspect = F3(
	function (config, file, context) {
		return A4(
			$stil4m$elm_syntax$Elm$Inspector$actionLambda,
			config.by,
			A2(
				$elm$core$Basics$composeR,
				A2($stil4m$elm_syntax$Elm$Inspector$inspectImports, config, file.eM),
				A2($stil4m$elm_syntax$Elm$Inspector$inspectDeclarations, config, file.a0)),
			file,
			context);
	});
var $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$startsWith = _String_startsWith;
var $stil4m$elm_syntax$Elm$Processing$Documentation$isDocumentationForRange = F2(
	function (range, _v0) {
		var commentRange = _v0.a;
		var commentText = _v0.b;
		if (A2($elm$core$String$startsWith, '{-|', commentText)) {
			var functionStartRow = range.fT.fD;
			return _Utils_eq(commentRange.eq.fD + 1, functionStartRow);
		} else {
			return false;
		}
	});
var $stil4m$elm_syntax$Elm$Processing$Documentation$replaceDeclaration = F2(
	function (_v0, _v1) {
		var r1 = _v0.a;
		var _new = _v0.b;
		var r2 = _v1.a;
		var old = _v1.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			r2,
			_Utils_eq(r1, r2) ? _new : old);
	});
var $stil4m$elm_syntax$Elm$Processing$Documentation$onFunction = F2(
	function (_v0, file) {
		var functionRange = _v0.a;
		var _function = _v0.b;
		var docs = A2(
			$elm$core$List$filter,
			$stil4m$elm_syntax$Elm$Processing$Documentation$isDocumentationForRange(functionRange),
			file.aX);
		var _v1 = $elm$core$List$head(docs);
		if (!_v1.$) {
			var doc = _v1.a;
			var docRange = doc.a;
			var docString = doc.b;
			return _Utils_update(
				file,
				{
					aX: A2(
						$elm$core$List$filter,
						$elm$core$Basics$neq(doc),
						file.aX),
					a0: A2(
						$elm$core$List$map,
						$stil4m$elm_syntax$Elm$Processing$Documentation$replaceDeclaration(
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								functionRange,
								$stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
									_Utils_update(
										_function,
										{
											ci: $elm$core$Maybe$Just(
												A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, docRange, docString))
										})))),
						file.a0)
				});
		} else {
			return file;
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration = function (a) {
	return {$: 2, a: a};
};
var $stil4m$elm_syntax$Elm$Processing$Documentation$onType = F2(
	function (_v0, file) {
		var r = _v0.a;
		var customType = _v0.b;
		var docs = A2(
			$elm$core$List$filter,
			$stil4m$elm_syntax$Elm$Processing$Documentation$isDocumentationForRange(r),
			file.aX);
		var _v1 = $elm$core$List$head(docs);
		if (!_v1.$) {
			var doc = _v1.a;
			var docRange = doc.a;
			var docString = doc.b;
			return _Utils_update(
				file,
				{
					aX: A2(
						$elm$core$List$filter,
						$elm$core$Basics$neq(doc),
						file.aX),
					a0: A2(
						$elm$core$List$map,
						$stil4m$elm_syntax$Elm$Processing$Documentation$replaceDeclaration(
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								r,
								$stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration(
									_Utils_update(
										customType,
										{
											ci: $elm$core$Maybe$Just(
												A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, docRange, docString))
										})))),
						file.a0)
				});
		} else {
			return file;
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Processing$Documentation$onTypeAlias = F2(
	function (_v0, file) {
		var r = _v0.a;
		var typeAlias = _v0.b;
		var docs = A2(
			$elm$core$List$filter,
			$stil4m$elm_syntax$Elm$Processing$Documentation$isDocumentationForRange(r),
			file.aX);
		var _v1 = $elm$core$List$head(docs);
		if (!_v1.$) {
			var doc = _v1.a;
			var docRange = doc.a;
			var docString = doc.b;
			return _Utils_update(
				file,
				{
					aX: A2(
						$elm$core$List$filter,
						$elm$core$Basics$neq(doc),
						file.aX),
					a0: A2(
						$elm$core$List$map,
						$stil4m$elm_syntax$Elm$Processing$Documentation$replaceDeclaration(
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								r,
								$stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration(
									_Utils_update(
										typeAlias,
										{
											ci: $elm$core$Maybe$Just(
												A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, docRange, docString))
										})))),
						file.a0)
				});
		} else {
			return file;
		}
	});
var $stil4m$elm_syntax$Elm$Processing$Documentation$postProcess = function (file) {
	return A3(
		$stil4m$elm_syntax$Elm$Inspector$inspect,
		_Utils_update(
			$stil4m$elm_syntax$Elm$Inspector$defaultConfig,
			{
				bz: $stil4m$elm_syntax$Elm$Inspector$Post($stil4m$elm_syntax$Elm$Processing$Documentation$onFunction),
				bK: $stil4m$elm_syntax$Elm$Inspector$Post($stil4m$elm_syntax$Elm$Processing$Documentation$onType),
				bL: $stil4m$elm_syntax$Elm$Inspector$Post($stil4m$elm_syntax$Elm$Processing$Documentation$onTypeAlias)
			}),
		file,
		file);
};
var $stil4m$elm_syntax$Elm$Interface$operators = $elm$core$List$filterMap(
	function (i) {
		if (i.$ === 3) {
			var o = i.a;
			return $elm$core$Maybe$Just(o);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$operator = function (t) {
	if (!t.$) {
		var s = t.a;
		return $elm$core$Maybe$Just(s);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$operators = function (l) {
	return A2($elm$core$List$filterMap, $stil4m$elm_syntax$Elm$Syntax$Exposing$operator, l);
};
var $stil4m$elm_syntax$Elm$Processing$buildSingle = F2(
	function (imp, moduleIndex) {
		var _v0 = imp.ew;
		if (_v0.$ === 1) {
			return _List_Nil;
		} else {
			if (!_v0.a.b.$) {
				var _v1 = _v0.a;
				return A2(
					$elm$core$List$map,
					function (x) {
						return _Utils_Tuple2(
							$stil4m$elm_syntax$Elm$Syntax$Node$value(x.o),
							x);
					},
					$stil4m$elm_syntax$Elm$Interface$operators(
						A2(
							$elm$core$Maybe$withDefault,
							_List_Nil,
							A2(
								$elm$core$Dict$get,
								$stil4m$elm_syntax$Elm$Syntax$Node$value(imp.bs),
								moduleIndex))));
			} else {
				var _v2 = _v0.a;
				var l = _v2.b.a;
				var selectedOperators = $stil4m$elm_syntax$Elm$Syntax$Exposing$operators(
					A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, l));
				return A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$first,
						function (elem) {
							return A2($elm$core$List$member, elem, selectedOperators);
						}),
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(x.o),
								x);
						},
						$stil4m$elm_syntax$Elm$Interface$operators(
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2(
									$elm$core$Dict$get,
									$stil4m$elm_syntax$Elm$Syntax$Node$value(imp.bs),
									moduleIndex)))));
			}
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$DefaultImports$defaults = _List_fromArray(
	[
		{
		ew: $elm$core$Maybe$Just(
			A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$All($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange))),
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Basics']))
	},
		{
		ew: $elm$core$Maybe$Just(
			A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
								A2($stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType, 'List', $elm$core$Maybe$Nothing))),
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose('::'))
						])))),
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['List']))
	},
		{
		ew: $elm$core$Maybe$Just(
			A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
								A2(
									$stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType,
									'Maybe',
									$elm$core$Maybe$Just($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange))))
						])))),
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Maybe']))
	},
		{
		ew: $elm$core$Maybe$Just(
			A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
								A2(
									$stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType,
									'Result',
									$elm$core$Maybe$Just($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange))))
						])))),
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Result']))
	},
		{
		ew: $elm$core$Maybe$Nothing,
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['String']))
	},
		{
		ew: $elm$core$Maybe$Nothing,
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Tuple']))
	},
		{
		ew: $elm$core$Maybe$Nothing,
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Debug']))
	},
		{
		ew: $elm$core$Maybe$Just(
			A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
								A2($stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType, 'Program', $elm$core$Maybe$Nothing)))
						])))),
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Platform']))
	},
		{
		ew: $elm$core$Maybe$Just(
			A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
								A2($stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType, 'Cmd', $elm$core$Maybe$Nothing))),
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose('!'))
						])))),
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Platform', 'Cmd']))
	},
		{
		ew: $elm$core$Maybe$Just(
			A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
								A2($stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType, 'Sub', $elm$core$Maybe$Nothing)))
						])))),
		e5: $elm$core$Maybe$Nothing,
		bs: A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$emptyRange,
			_List_fromArray(
				['Platform', 'Sub']))
	}
	]);
var $stil4m$elm_syntax$Elm$Processing$tableForFile = F2(
	function (rawFile, _v0) {
		var moduleIndex = _v0;
		return $elm$core$Dict$fromList(
			A2(
				$elm$core$List$concatMap,
				function (a) {
					return A2($stil4m$elm_syntax$Elm$Processing$buildSingle, a, moduleIndex);
				},
				_Utils_ap(
					$stil4m$elm_syntax$Elm$DefaultImports$defaults,
					$stil4m$elm_syntax$Elm$RawFile$imports(rawFile))));
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression = function (a) {
	return {$: 16, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression = function (a) {
	return {$: 17, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression = function (a) {
	return {$: 15, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetFunction = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr = function (a) {
	return {$: 19, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression = function (a) {
	return {$: 14, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr = function (a) {
	return {$: 18, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression = F2(
	function (a, b) {
		return {$: 22, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression = function (a) {
	return {$: 13, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Node$map = F2(
	function (f, _v0) {
		var r = _v0.a;
		var a = _v0.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			r,
			f(a));
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $stil4m$elm_syntax$Elm$Processing$visitExpression = F3(
	function (visitor, context, expression) {
		var inner = A2($stil4m$elm_syntax$Elm$Processing$visitExpressionInner, visitor, context);
		return A3(
			A2(
				$elm$core$Maybe$withDefault,
				F3(
					function (_v4, nest, expr) {
						return nest(expr);
					}),
				visitor),
			context,
			inner,
			expression);
	});
var $stil4m$elm_syntax$Elm$Processing$visitExpressionInner = F3(
	function (visitor, context, _v2) {
		var range = _v2.a;
		var expression = _v2.b;
		var subVisit = A2($stil4m$elm_syntax$Elm$Processing$visitExpression, visitor, context);
		return function (newExpr) {
			return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, newExpr);
		}(
			function () {
				switch (expression.$) {
					case 1:
						var expressionList = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$Application(
							A2($elm$core$List$map, subVisit, expressionList));
					case 2:
						var op = expression.a;
						var dir = expression.b;
						var left = expression.c;
						var right = expression.d;
						return A4(
							$stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication,
							op,
							dir,
							subVisit(left),
							subVisit(right));
					case 4:
						var e1 = expression.a;
						var e2 = expression.b;
						var e3 = expression.c;
						return A3(
							$stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock,
							subVisit(e1),
							subVisit(e2),
							subVisit(e3));
					case 13:
						var expressionList = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
							A2($elm$core$List$map, subVisit, expressionList));
					case 14:
						var expr1 = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(
							subVisit(expr1));
					case 15:
						var letBlock = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression(
							{
								a0: A3($stil4m$elm_syntax$Elm$Processing$visitLetDeclarations, visitor, context, letBlock.a0),
								_: subVisit(letBlock._)
							});
					case 16:
						var caseBlock = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression(
							{
								b8: A2(
									$elm$core$List$map,
									$elm$core$Tuple$mapSecond(subVisit),
									caseBlock.b8),
								_: subVisit(caseBlock._)
							});
					case 17:
						var lambda = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression(
							_Utils_update(
								lambda,
								{
									_: subVisit(lambda._)
								}));
					case 18:
						var expressionStringList = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr(
							A2(
								$elm$core$List$map,
								$stil4m$elm_syntax$Elm$Syntax$Node$map(
									$elm$core$Tuple$mapSecond(subVisit)),
								expressionStringList));
					case 19:
						var expressionList = expression.a;
						return $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr(
							A2($elm$core$List$map, subVisit, expressionList));
					case 22:
						var name = expression.a;
						var updates = expression.b;
						return A2(
							$stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression,
							name,
							A2(
								$elm$core$List$map,
								$stil4m$elm_syntax$Elm$Syntax$Node$map(
									$elm$core$Tuple$mapSecond(subVisit)),
								updates));
					default:
						return expression;
				}
			}());
	});
var $stil4m$elm_syntax$Elm$Processing$visitFunctionDecl = F3(
	function (visitor, context, _function) {
		var newFunctionDeclaration = A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$map,
			A2($stil4m$elm_syntax$Elm$Processing$visitFunctionDeclaration, visitor, context),
			_function.am);
		return _Utils_update(
			_function,
			{am: newFunctionDeclaration});
	});
var $stil4m$elm_syntax$Elm$Processing$visitFunctionDeclaration = F3(
	function (visitor, context, functionDeclaration) {
		var newExpression = A3($stil4m$elm_syntax$Elm$Processing$visitExpression, visitor, context, functionDeclaration._);
		return _Utils_update(
			functionDeclaration,
			{_: newExpression});
	});
var $stil4m$elm_syntax$Elm$Processing$visitLetDeclaration = F3(
	function (visitor, context, _v0) {
		var range = _v0.a;
		var declaration = _v0.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			range,
			function () {
				if (!declaration.$) {
					var _function = declaration.a;
					return $stil4m$elm_syntax$Elm$Syntax$Expression$LetFunction(
						A3($stil4m$elm_syntax$Elm$Processing$visitFunctionDecl, visitor, context, _function));
				} else {
					var pattern = declaration.a;
					var expression = declaration.b;
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring,
						pattern,
						A3($stil4m$elm_syntax$Elm$Processing$visitExpression, visitor, context, expression));
				}
			}());
	});
var $stil4m$elm_syntax$Elm$Processing$visitLetDeclarations = F3(
	function (visitor, context, declarations) {
		return A2(
			$elm$core$List$map,
			A2($stil4m$elm_syntax$Elm$Processing$visitLetDeclaration, visitor, context),
			declarations);
	});
var $stil4m$elm_syntax$Elm$Processing$visitDeclaration = F3(
	function (visitor, context, _v0) {
		var range = _v0.a;
		var declaration = _v0.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			range,
			function () {
				if (!declaration.$) {
					var _function = declaration.a;
					return $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
						A3($stil4m$elm_syntax$Elm$Processing$visitFunctionDecl, visitor, context, _function));
				} else {
					return declaration;
				}
			}());
	});
var $stil4m$elm_syntax$Elm$Processing$visitDeclarations = F3(
	function (visitor, context, declarations) {
		return A2(
			$elm$core$List$map,
			A2($stil4m$elm_syntax$Elm$Processing$visitDeclaration, visitor, context),
			declarations);
	});
var $stil4m$elm_syntax$Elm$Processing$visit = F3(
	function (visitor, context, file) {
		var newDeclarations = A3($stil4m$elm_syntax$Elm$Processing$visitDeclarations, visitor, context, file.a0);
		return _Utils_update(
			file,
			{a0: newDeclarations});
	});
var $stil4m$elm_syntax$Elm$Processing$process = F2(
	function (processContext, rawFile) {
		var file = rawFile;
		var table = A2($stil4m$elm_syntax$Elm$Processing$tableForFile, rawFile, processContext);
		var operatorFixed = A3(
			$stil4m$elm_syntax$Elm$Processing$visit,
			$elm$core$Maybe$Just(
				F3(
					function (context, inner, expression) {
						return inner(
							function () {
								if (expression.b.$ === 1) {
									var r = expression.a;
									var args = expression.b.a;
									return A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										r,
										A2($stil4m$elm_syntax$Elm$Processing$fixApplication, context, args));
								} else {
									return expression;
								}
							}());
					})),
			table,
			file);
		var documentationFixed = $stil4m$elm_syntax$Elm$Processing$Documentation$postProcess(operatorFixed);
		return documentationFixed;
	});
var $stil4m$elm_syntax$Elm$Processing$addDependency = F2(
	function (dep, _v0) {
		var x = _v0;
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A3($elm$core$Dict$insert, k, v, d);
				}),
			x,
			dep.eV);
	});
var $stil4m$elm_syntax$Elm$Syntax$Infix$Non = 2;
var $stil4m$elm_syntax$Elm$Syntax$Infix$Right = 1;
var $author$project$Morphir$Elm$WellKnownOperators$elmCore = {
	eV: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				_List_fromArray(
					['Array']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Array', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('empty'),
						$stil4m$elm_syntax$Elm$Interface$Function('isEmpty'),
						$stil4m$elm_syntax$Elm$Interface$Function('length'),
						$stil4m$elm_syntax$Elm$Interface$Function('initialize'),
						$stil4m$elm_syntax$Elm$Interface$Function('repeat'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromList'),
						$stil4m$elm_syntax$Elm$Interface$Function('get'),
						$stil4m$elm_syntax$Elm$Interface$Function('set'),
						$stil4m$elm_syntax$Elm$Interface$Function('push'),
						$stil4m$elm_syntax$Elm$Interface$Function('toList'),
						$stil4m$elm_syntax$Elm$Interface$Function('toIndexedList'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldr'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldl'),
						$stil4m$elm_syntax$Elm$Interface$Function('filter'),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('indexedMap'),
						$stil4m$elm_syntax$Elm$Interface$Function('append'),
						$stil4m$elm_syntax$Elm$Interface$Function('slice')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Basics']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Int', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Float', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'add'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '+'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 6)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'sub'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '-'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 6)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'mul'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '*'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 7)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'fdiv'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '/'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 7)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'idiv'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '//'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 7)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 1),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'pow'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '^'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 8)
						}),
						$stil4m$elm_syntax$Elm$Interface$Function('toFloat'),
						$stil4m$elm_syntax$Elm$Interface$Function('round'),
						$stil4m$elm_syntax$Elm$Interface$Function('floor'),
						$stil4m$elm_syntax$Elm$Interface$Function('ceiling'),
						$stil4m$elm_syntax$Elm$Interface$Function('truncate'),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 2),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'eq'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '=='),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 4)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 2),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'neq'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '/='),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 4)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 2),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'lt'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '<'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 4)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 2),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'gt'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '>'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 4)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 2),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'le'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '<='),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 4)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 2),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'ge'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '>='),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 4)
						}),
						$stil4m$elm_syntax$Elm$Interface$Function('max'),
						$stil4m$elm_syntax$Elm$Interface$Function('min'),
						$stil4m$elm_syntax$Elm$Interface$Function('compare'),
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2(
							'Order',
							_List_fromArray(
								['LT', 'EQ', 'GT']))),
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2(
							'Bool',
							_List_fromArray(
								['True', 'False']))),
						$stil4m$elm_syntax$Elm$Interface$Function('not'),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 1),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'and'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '&&'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 3)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 1),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'or'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '||'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 2)
						}),
						$stil4m$elm_syntax$Elm$Interface$Function('xor'),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 1),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'append'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '++'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 5)
						}),
						$stil4m$elm_syntax$Elm$Interface$Function('modBy'),
						$stil4m$elm_syntax$Elm$Interface$Function('remainderBy'),
						$stil4m$elm_syntax$Elm$Interface$Function('negate'),
						$stil4m$elm_syntax$Elm$Interface$Function('abs'),
						$stil4m$elm_syntax$Elm$Interface$Function('clamp'),
						$stil4m$elm_syntax$Elm$Interface$Function('sqrt'),
						$stil4m$elm_syntax$Elm$Interface$Function('logBase'),
						$stil4m$elm_syntax$Elm$Interface$Function('e'),
						$stil4m$elm_syntax$Elm$Interface$Function('pi'),
						$stil4m$elm_syntax$Elm$Interface$Function('cos'),
						$stil4m$elm_syntax$Elm$Interface$Function('sin'),
						$stil4m$elm_syntax$Elm$Interface$Function('tan'),
						$stil4m$elm_syntax$Elm$Interface$Function('acos'),
						$stil4m$elm_syntax$Elm$Interface$Function('asin'),
						$stil4m$elm_syntax$Elm$Interface$Function('atan'),
						$stil4m$elm_syntax$Elm$Interface$Function('atan2'),
						$stil4m$elm_syntax$Elm$Interface$Function('degrees'),
						$stil4m$elm_syntax$Elm$Interface$Function('radians'),
						$stil4m$elm_syntax$Elm$Interface$Function('turns'),
						$stil4m$elm_syntax$Elm$Interface$Function('toPolar'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromPolar'),
						$stil4m$elm_syntax$Elm$Interface$Function('isNaN'),
						$stil4m$elm_syntax$Elm$Interface$Function('isInfinite'),
						$stil4m$elm_syntax$Elm$Interface$Function('identity'),
						$stil4m$elm_syntax$Elm$Interface$Function('always'),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 1),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'apL'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '<|'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'apR'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '|>'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 0),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'composeL'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '<<'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 9)
						}),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 1),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'composeR'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '>>'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 9)
						}),
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Never', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('never')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Bitwise']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$Function('and'),
						$stil4m$elm_syntax$Elm$Interface$Function('or'),
						$stil4m$elm_syntax$Elm$Interface$Function('xor'),
						$stil4m$elm_syntax$Elm$Interface$Function('complement'),
						$stil4m$elm_syntax$Elm$Interface$Function('shiftLeftBy'),
						$stil4m$elm_syntax$Elm$Interface$Function('shiftRightBy'),
						$stil4m$elm_syntax$Elm$Interface$Function('shiftRightZfBy')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Char']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Char', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('isUpper'),
						$stil4m$elm_syntax$Elm$Interface$Function('isLower'),
						$stil4m$elm_syntax$Elm$Interface$Function('isAlpha'),
						$stil4m$elm_syntax$Elm$Interface$Function('isAlphaNum'),
						$stil4m$elm_syntax$Elm$Interface$Function('isDigit'),
						$stil4m$elm_syntax$Elm$Interface$Function('isOctDigit'),
						$stil4m$elm_syntax$Elm$Interface$Function('isHexDigit'),
						$stil4m$elm_syntax$Elm$Interface$Function('toUpper'),
						$stil4m$elm_syntax$Elm$Interface$Function('toLower'),
						$stil4m$elm_syntax$Elm$Interface$Function('toLocaleUpper'),
						$stil4m$elm_syntax$Elm$Interface$Function('toLocaleLower'),
						$stil4m$elm_syntax$Elm$Interface$Function('toCode'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromCode')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Debug']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$Function('toString'),
						$stil4m$elm_syntax$Elm$Interface$Function('log'),
						$stil4m$elm_syntax$Elm$Interface$Function('todo')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Dict']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Dict', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('empty'),
						$stil4m$elm_syntax$Elm$Interface$Function('singleton'),
						$stil4m$elm_syntax$Elm$Interface$Function('insert'),
						$stil4m$elm_syntax$Elm$Interface$Function('update'),
						$stil4m$elm_syntax$Elm$Interface$Function('remove'),
						$stil4m$elm_syntax$Elm$Interface$Function('isEmpty'),
						$stil4m$elm_syntax$Elm$Interface$Function('member'),
						$stil4m$elm_syntax$Elm$Interface$Function('get'),
						$stil4m$elm_syntax$Elm$Interface$Function('size'),
						$stil4m$elm_syntax$Elm$Interface$Function('keys'),
						$stil4m$elm_syntax$Elm$Interface$Function('values'),
						$stil4m$elm_syntax$Elm$Interface$Function('toList'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromList'),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldl'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldr'),
						$stil4m$elm_syntax$Elm$Interface$Function('filter'),
						$stil4m$elm_syntax$Elm$Interface$Function('partition'),
						$stil4m$elm_syntax$Elm$Interface$Function('union'),
						$stil4m$elm_syntax$Elm$Interface$Function('intersect'),
						$stil4m$elm_syntax$Elm$Interface$Function('diff'),
						$stil4m$elm_syntax$Elm$Interface$Function('merge')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['List']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$Function('singleton'),
						$stil4m$elm_syntax$Elm$Interface$Function('repeat'),
						$stil4m$elm_syntax$Elm$Interface$Function('range'),
						$stil4m$elm_syntax$Elm$Interface$Operator(
						{
							m: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 1),
							n: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 'cons'),
							o: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, '::'),
							q: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, 5)
						}),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('indexedMap'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldl'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldr'),
						$stil4m$elm_syntax$Elm$Interface$Function('filter'),
						$stil4m$elm_syntax$Elm$Interface$Function('filterMap'),
						$stil4m$elm_syntax$Elm$Interface$Function('length'),
						$stil4m$elm_syntax$Elm$Interface$Function('reverse'),
						$stil4m$elm_syntax$Elm$Interface$Function('member'),
						$stil4m$elm_syntax$Elm$Interface$Function('all'),
						$stil4m$elm_syntax$Elm$Interface$Function('any'),
						$stil4m$elm_syntax$Elm$Interface$Function('maximum'),
						$stil4m$elm_syntax$Elm$Interface$Function('minimum'),
						$stil4m$elm_syntax$Elm$Interface$Function('sum'),
						$stil4m$elm_syntax$Elm$Interface$Function('product'),
						$stil4m$elm_syntax$Elm$Interface$Function('append'),
						$stil4m$elm_syntax$Elm$Interface$Function('concat'),
						$stil4m$elm_syntax$Elm$Interface$Function('concatMap'),
						$stil4m$elm_syntax$Elm$Interface$Function('intersperse'),
						$stil4m$elm_syntax$Elm$Interface$Function('map2'),
						$stil4m$elm_syntax$Elm$Interface$Function('map3'),
						$stil4m$elm_syntax$Elm$Interface$Function('map4'),
						$stil4m$elm_syntax$Elm$Interface$Function('map5'),
						$stil4m$elm_syntax$Elm$Interface$Function('sort'),
						$stil4m$elm_syntax$Elm$Interface$Function('sortBy'),
						$stil4m$elm_syntax$Elm$Interface$Function('sortWith'),
						$stil4m$elm_syntax$Elm$Interface$Function('isEmpty'),
						$stil4m$elm_syntax$Elm$Interface$Function('head'),
						$stil4m$elm_syntax$Elm$Interface$Function('tail'),
						$stil4m$elm_syntax$Elm$Interface$Function('take'),
						$stil4m$elm_syntax$Elm$Interface$Function('drop'),
						$stil4m$elm_syntax$Elm$Interface$Function('partition'),
						$stil4m$elm_syntax$Elm$Interface$Function('unzip')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Maybe']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2(
							'Maybe',
							_List_fromArray(
								['Just', 'Nothing']))),
						$stil4m$elm_syntax$Elm$Interface$Function('andThen'),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('map2'),
						$stil4m$elm_syntax$Elm$Interface$Function('map3'),
						$stil4m$elm_syntax$Elm$Interface$Function('map4'),
						$stil4m$elm_syntax$Elm$Interface$Function('map5'),
						$stil4m$elm_syntax$Elm$Interface$Function('withDefault')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Platform']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Program', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('worker'),
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Task', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('ProcessId', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Router', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('sendToApp'),
						$stil4m$elm_syntax$Elm$Interface$Function('sendToSelf')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Platform', 'Cmd']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Cmd', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('none'),
						$stil4m$elm_syntax$Elm$Interface$Function('batch'),
						$stil4m$elm_syntax$Elm$Interface$Function('map')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Platform', 'Sub']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Sub', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('none'),
						$stil4m$elm_syntax$Elm$Interface$Function('batch'),
						$stil4m$elm_syntax$Elm$Interface$Function('map')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Process']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$Alias('Id'),
						$stil4m$elm_syntax$Elm$Interface$Function('spawn'),
						$stil4m$elm_syntax$Elm$Interface$Function('sleep'),
						$stil4m$elm_syntax$Elm$Interface$Function('kill')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Result']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2(
							'Result',
							_List_fromArray(
								['Ok', 'Err']))),
						$stil4m$elm_syntax$Elm$Interface$Function('withDefault'),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('map2'),
						$stil4m$elm_syntax$Elm$Interface$Function('map3'),
						$stil4m$elm_syntax$Elm$Interface$Function('map4'),
						$stil4m$elm_syntax$Elm$Interface$Function('map5'),
						$stil4m$elm_syntax$Elm$Interface$Function('andThen'),
						$stil4m$elm_syntax$Elm$Interface$Function('toMaybe'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromMaybe'),
						$stil4m$elm_syntax$Elm$Interface$Function('mapError')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Set']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('Set', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('empty'),
						$stil4m$elm_syntax$Elm$Interface$Function('singleton'),
						$stil4m$elm_syntax$Elm$Interface$Function('insert'),
						$stil4m$elm_syntax$Elm$Interface$Function('remove'),
						$stil4m$elm_syntax$Elm$Interface$Function('isEmpty'),
						$stil4m$elm_syntax$Elm$Interface$Function('member'),
						$stil4m$elm_syntax$Elm$Interface$Function('size'),
						$stil4m$elm_syntax$Elm$Interface$Function('union'),
						$stil4m$elm_syntax$Elm$Interface$Function('intersect'),
						$stil4m$elm_syntax$Elm$Interface$Function('diff'),
						$stil4m$elm_syntax$Elm$Interface$Function('toList'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromList'),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldl'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldr'),
						$stil4m$elm_syntax$Elm$Interface$Function('filter'),
						$stil4m$elm_syntax$Elm$Interface$Function('partition')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['String']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$CustomType(
						_Utils_Tuple2('String', _List_Nil)),
						$stil4m$elm_syntax$Elm$Interface$Function('isEmpty'),
						$stil4m$elm_syntax$Elm$Interface$Function('length'),
						$stil4m$elm_syntax$Elm$Interface$Function('reverse'),
						$stil4m$elm_syntax$Elm$Interface$Function('repeat'),
						$stil4m$elm_syntax$Elm$Interface$Function('replace'),
						$stil4m$elm_syntax$Elm$Interface$Function('append'),
						$stil4m$elm_syntax$Elm$Interface$Function('concat'),
						$stil4m$elm_syntax$Elm$Interface$Function('split'),
						$stil4m$elm_syntax$Elm$Interface$Function('join'),
						$stil4m$elm_syntax$Elm$Interface$Function('words'),
						$stil4m$elm_syntax$Elm$Interface$Function('lines'),
						$stil4m$elm_syntax$Elm$Interface$Function('slice'),
						$stil4m$elm_syntax$Elm$Interface$Function('left'),
						$stil4m$elm_syntax$Elm$Interface$Function('right'),
						$stil4m$elm_syntax$Elm$Interface$Function('dropLeft'),
						$stil4m$elm_syntax$Elm$Interface$Function('dropRight'),
						$stil4m$elm_syntax$Elm$Interface$Function('contains'),
						$stil4m$elm_syntax$Elm$Interface$Function('startsWith'),
						$stil4m$elm_syntax$Elm$Interface$Function('endsWith'),
						$stil4m$elm_syntax$Elm$Interface$Function('indexes'),
						$stil4m$elm_syntax$Elm$Interface$Function('indices'),
						$stil4m$elm_syntax$Elm$Interface$Function('toInt'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromInt'),
						$stil4m$elm_syntax$Elm$Interface$Function('toFloat'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromFloat'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromChar'),
						$stil4m$elm_syntax$Elm$Interface$Function('cons'),
						$stil4m$elm_syntax$Elm$Interface$Function('uncons'),
						$stil4m$elm_syntax$Elm$Interface$Function('toList'),
						$stil4m$elm_syntax$Elm$Interface$Function('fromList'),
						$stil4m$elm_syntax$Elm$Interface$Function('toUpper'),
						$stil4m$elm_syntax$Elm$Interface$Function('toLower'),
						$stil4m$elm_syntax$Elm$Interface$Function('pad'),
						$stil4m$elm_syntax$Elm$Interface$Function('padLeft'),
						$stil4m$elm_syntax$Elm$Interface$Function('padRight'),
						$stil4m$elm_syntax$Elm$Interface$Function('trim'),
						$stil4m$elm_syntax$Elm$Interface$Function('trimLeft'),
						$stil4m$elm_syntax$Elm$Interface$Function('trimRight'),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('filter'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldl'),
						$stil4m$elm_syntax$Elm$Interface$Function('foldr'),
						$stil4m$elm_syntax$Elm$Interface$Function('any'),
						$stil4m$elm_syntax$Elm$Interface$Function('all')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Task']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$Alias('Task'),
						$stil4m$elm_syntax$Elm$Interface$Function('succeed'),
						$stil4m$elm_syntax$Elm$Interface$Function('fail'),
						$stil4m$elm_syntax$Elm$Interface$Function('map'),
						$stil4m$elm_syntax$Elm$Interface$Function('map2'),
						$stil4m$elm_syntax$Elm$Interface$Function('map3'),
						$stil4m$elm_syntax$Elm$Interface$Function('map4'),
						$stil4m$elm_syntax$Elm$Interface$Function('map5'),
						$stil4m$elm_syntax$Elm$Interface$Function('sequence'),
						$stil4m$elm_syntax$Elm$Interface$Function('andThen'),
						$stil4m$elm_syntax$Elm$Interface$Function('onError'),
						$stil4m$elm_syntax$Elm$Interface$Function('mapError'),
						$stil4m$elm_syntax$Elm$Interface$Function('perform'),
						$stil4m$elm_syntax$Elm$Interface$Function('attempt')
					])),
				_Utils_Tuple2(
				_List_fromArray(
					['Tuple']),
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Interface$Function('pair'),
						$stil4m$elm_syntax$Elm$Interface$Function('first'),
						$stil4m$elm_syntax$Elm$Interface$Function('second'),
						$stil4m$elm_syntax$Elm$Interface$Function('mapFirst'),
						$stil4m$elm_syntax$Elm$Interface$Function('mapSecond'),
						$stil4m$elm_syntax$Elm$Interface$Function('mapBoth')
					]))
			])),
	cN: 'elm/core',
	gs: '1.0.5'
};
var $author$project$Morphir$Elm$WellKnownOperators$wellKnownOperators = _List_fromArray(
	[$author$project$Morphir$Elm$WellKnownOperators$elmCore]);
var $author$project$Morphir$Elm$Frontend$withWellKnownOperators = function (context) {
	return A3($elm$core$List$foldl, $stil4m$elm_syntax$Elm$Processing$addDependency, context, $author$project$Morphir$Elm$WellKnownOperators$wellKnownOperators);
};
var $author$project$Morphir$Elm$Frontend$mapParsedFiles = F4(
	function (dependencies, currentPackagePath, parsedModules, sortedModuleNames) {
		var initialContext = $author$project$Morphir$Elm$Frontend$withWellKnownOperators($stil4m$elm_syntax$Elm$Processing$init);
		return A2(
			$elm$core$Result$map,
			$elm$core$Tuple$second,
			A3(
				$elm$core$List$foldl,
				F2(
					function (parsedFile, moduleResultsSoFar) {
						return A2(
							$elm$core$Result$andThen,
							function (_v0) {
								var processContext = _v0.a;
								var modulesSoFar = _v0.b;
								var processedFile = A2($stil4m$elm_syntax$Elm$Processing$process, processContext, parsedFile.aN);
								var newProcessContext = A2($stil4m$elm_syntax$Elm$Processing$addFile, parsedFile.aN, processContext);
								return A2(
									$elm$core$Result$map,
									$elm$core$Tuple$pair(newProcessContext),
									A4(
										$author$project$Morphir$Elm$Frontend$mapProcessedFile,
										dependencies,
										currentPackagePath,
										A2($author$project$Morphir$Elm$Frontend$ProcessedFile, parsedFile, processedFile),
										modulesSoFar));
							},
							moduleResultsSoFar);
					}),
				$elm$core$Result$Ok(
					_Utils_Tuple2(initialContext, $elm$core$Dict$empty)),
				A2(
					$elm$core$List$filterMap,
					function (moduleName) {
						return A2($elm$core$Dict$get, moduleName, parsedModules);
					},
					sortedModuleNames)));
	});
var $stil4m$elm_syntax$Elm$Parser$State$State = $elm$core$Basics$identity;
var $stil4m$elm_syntax$Elm$Parser$State$emptyState = {aX: _List_Nil, ap: _List_Nil};
var $stil4m$elm_syntax$Elm$Syntax$File$File = F4(
	function (moduleDefinition, imports, declarations, comments) {
		return {aX: comments, a0: declarations, eM: imports, cL: moduleDefinition};
	});
var $stil4m$elm_syntax$Combine$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0;
		return function (s0) {
			var _v1 = parseA(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				var _v2 = callback(a);
				var parseB = _v2;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
				}
			}
		};
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $stil4m$elm_syntax$Combine$andMap = F2(
	function (_v0, _v1) {
		var rp = _v0;
		var lp = _v1;
		return function (state) {
			return A2(
				$elm$parser$Parser$andThen,
				function (_v2) {
					var newState = _v2.a;
					var a = _v2.b;
					return A2(
						$elm$parser$Parser$map,
						$elm$core$Tuple$mapSecond(a),
						rp(newState));
				},
				lp(state));
		};
	});
var $stil4m$elm_syntax$Elm$Parser$State$getComments = function (_v0) {
	var s = _v0;
	return s.aX;
};
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $stil4m$elm_syntax$Combine$succeed = function (res) {
	return function (state) {
		return $elm$parser$Parser$succeed(
			_Utils_Tuple2(state, res));
	};
};
var $stil4m$elm_syntax$Combine$withState = function (f) {
	return function (state) {
		return function (_v0) {
			var p = _v0;
			return p(state);
		}(
			f(state));
	};
};
var $stil4m$elm_syntax$Elm$Parser$File$collectComments = $stil4m$elm_syntax$Combine$withState(
	A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Parser$State$getComments, $stil4m$elm_syntax$Combine$succeed));
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (!_v1.$) {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $stil4m$elm_syntax$Combine$choice = function (xs) {
	return function (state) {
		return $elm$parser$Parser$oneOf(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var x = _v0;
					return x(state);
				},
				xs));
	};
};
var $stil4m$elm_syntax$Elm$Syntax$Declaration$Destructuring = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Node$combine = F3(
	function (f, a, b) {
		var r1 = a.a;
		var r2 = b.a;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$combine(
				_List_fromArray(
					[r1, r2])),
			A2(f, a, b));
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$CaseBlock = F2(
	function (expression, cases) {
		return {b8: cases, _: expression};
	});
var $stil4m$elm_syntax$Combine$Done = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Function = F3(
	function (documentation, signature, declaration) {
		return {am: declaration, ci: documentation, fL: signature};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$FunctionImplementation = F3(
	function (name, _arguments, expression) {
		return {b4: _arguments, _: expression, cN: name};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$Lambda = F2(
	function (args, expression) {
		return {dP: args, _: expression};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetBlock = F2(
	function (declarations, expression) {
		return {a0: declarations, _: expression};
	});
var $stil4m$elm_syntax$Combine$Loop = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Negation = function (a) {
	return {$: 10, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Operator = function (a) {
	return {$: 6, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$PrefixOperator = function (a) {
	return {$: 5, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr = {$: 0};
var $stil4m$elm_syntax$Combine$andThen = F2(
	function (f, _v0) {
		var p = _v0;
		return function (state) {
			return A2(
				$elm$parser$Parser$andThen,
				function (_v1) {
					var s = _v1.a;
					var a = _v1.b;
					return function (_v2) {
						var x = _v2;
						return x(s);
					}(
						f(a));
				},
				p(state));
		};
	});
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0;
	return function (s0) {
		var _v1 = parse(s0);
		if (_v1.$ === 1) {
			var x = _v1.b;
			return A2($elm$parser$Parser$Advanced$Bad, false, x);
		} else {
			var a = _v1.b;
			var s1 = _v1.c;
			return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
		}
	};
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $stil4m$elm_syntax$Combine$backtrackable = function (_v0) {
	var p = _v0;
	return function (state) {
		return $elm$parser$Parser$backtrackable(
			p(state));
	};
};
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					A2(
						func,
						A3($elm$core$String$slice, s0.b, s1.b, s0.a),
						a),
					s1);
			}
		};
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {eb: col, eg: contextStack, fx: problem, fD: row};
	});
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.fD, s.eb, x, s.f));
	});
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.b, s.fD, s.eb, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{eb: newCol, f: s.f, j: s.j, b: newOffset, fD: newRow, a: s.a});
	};
};
var $elm$parser$Parser$token = function (str) {
	return $elm$parser$Parser$Advanced$token(
		$elm$parser$Parser$toToken(str));
};
var $stil4m$elm_syntax$Combine$string = function (s) {
	return function (state) {
		return A2(
			$elm$parser$Parser$map,
			function (x) {
				return _Utils_Tuple2(state, x);
			},
			$elm$parser$Parser$getChompedString(
				$elm$parser$Parser$token(s)));
	};
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$caseToken = $stil4m$elm_syntax$Combine$string('case');
var $stil4m$elm_syntax$Elm$Syntax$Expression$CharLiteral = function (a) {
	return {$: 12, a: a};
};
var $elm$parser$Parser$Problem = function (a) {
	return {$: 12, a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return function (s) {
		return A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $stil4m$elm_syntax$Combine$fail = function (m) {
	return function (state) {
		return A2(
			$elm$parser$Parser$map,
			function (x) {
				return _Utils_Tuple2(state, x);
			},
			$elm$parser$Parser$problem(m));
	};
};
var $elm$parser$Parser$UnexpectedChar = {$: 11};
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return function (s) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.b, s.a);
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{eb: 1, f: s.f, j: s.j, b: s.b + 1, fD: s.fD + 1, a: s.a}) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{eb: s.eb + 1, f: s.f, j: s.j, b: newOffset, fD: s.fD, a: s.a}));
		};
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
			var _v2 = parseA(s0);
			if (_v2.$ === 1) {
				var p = _v2.a;
				var x = _v2.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v2.a;
				var a = _v2.b;
				var s1 = _v2.c;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $stil4m$elm_syntax$Combine$fromCore = function (p) {
	return function (state) {
		return A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed(
				function (v) {
					return _Utils_Tuple2(state, v);
				}),
			p);
	};
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $stil4m$elm_syntax$Combine$Char$satisfy = function (pred) {
	return $stil4m$elm_syntax$Combine$fromCore(
		A2(
			$elm$parser$Parser$andThen,
			function (s) {
				var _v0 = $elm$core$String$toList(s);
				if (!_v0.b) {
					return $elm$parser$Parser$succeed($elm$core$Maybe$Nothing);
				} else {
					var c = _v0.a;
					return $elm$parser$Parser$succeed(
						$elm$core$Maybe$Just(c));
				}
			},
			$elm$parser$Parser$getChompedString(
				$elm$parser$Parser$chompIf(pred))));
};
var $stil4m$elm_syntax$Combine$Char$anyChar = A2(
	$stil4m$elm_syntax$Combine$andThen,
	A2(
		$elm$core$Basics$composeR,
		$elm$core$Maybe$map($stil4m$elm_syntax$Combine$succeed),
		$elm$core$Maybe$withDefault(
			$stil4m$elm_syntax$Combine$fail('expected any character'))),
	$stil4m$elm_syntax$Combine$Char$satisfy(
		$elm$core$Basics$always(true)));
var $elm$core$String$fromList = _String_fromList;
var $stil4m$elm_syntax$Combine$Char$char = function (c) {
	return A2(
		$stil4m$elm_syntax$Combine$andThen,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Maybe$map($stil4m$elm_syntax$Combine$succeed),
			$elm$core$Maybe$withDefault(
				$stil4m$elm_syntax$Combine$fail(
					'expected \'' + ($elm$core$String$fromList(
						_List_fromArray(
							[c])) + '\'')))),
		$stil4m$elm_syntax$Combine$Char$satisfy(
			$elm$core$Basics$eq(c)));
};
var $stil4m$elm_syntax$Combine$map = F2(
	function (f, _v0) {
		var p = _v0;
		return function (state) {
			return A2(
				$elm$parser$Parser$map,
				function (_v1) {
					var s = _v1.a;
					var a = _v1.b;
					return _Utils_Tuple2(
						s,
						f(a));
				},
				p(state));
		};
	});
var $stil4m$elm_syntax$Combine$continueWith = F2(
	function (target, dropped) {
		return A2(
			$stil4m$elm_syntax$Combine$andMap,
			target,
			A2(
				$stil4m$elm_syntax$Combine$map,
				F2(
					function (b, a) {
						return A2($elm$core$Basics$always, a, b);
					}),
				dropped));
	});
var $stil4m$elm_syntax$Combine$ignore = F2(
	function (dropped, target) {
		return A2(
			$stil4m$elm_syntax$Combine$andMap,
			dropped,
			A2($stil4m$elm_syntax$Combine$map, $elm$core$Basics$always, target));
	});
var $stil4m$elm_syntax$Combine$or = F2(
	function (_v0, _v1) {
		var lp = _v0;
		var rp = _v1;
		return function (state) {
			return $elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						lp(state),
						rp(state)
					]));
		};
	});
var $elm$core$String$any = _String_any;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.a);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.b, offset) < 0,
					0,
					{eb: col, f: s0.f, j: s0.j, b: offset, fD: row, a: s0.a});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.b, s.fD, s.eb, s);
	};
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$core$Char$fromCode = _Char_fromCode;
var $elm$core$Basics$pow = _Basics_pow;
var $rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return $elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2($elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return $elm$core$Result$Err(
							$elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $rtfeldman$elm_hex$Hex$fromString = function (str) {
	if ($elm$core$String$isEmpty(str)) {
		return $elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2($elm$core$String$startsWith, '-', str)) {
				var list = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$tail(
						$elm$core$String$toList(str)));
				return A2(
					$elm$core$Result$map,
					$elm$core$Basics$negate,
					A3(
						$rtfeldman$elm_hex$Hex$fromStringHelp,
						$elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					$rtfeldman$elm_hex$Hex$fromStringHelp,
					$elm$core$String$length(str) - 1,
					$elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				$elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2($elm$core$Result$mapError, formatError, result);
	}
};
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValue = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed('\''),
			$elm$parser$Parser$symbol('\'')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed('\"'),
			$elm$parser$Parser$symbol('\"')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed('\n'),
			$elm$parser$Parser$symbol('n')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed('\t'),
			$elm$parser$Parser$symbol('t')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed('\u000D'),
			$elm$parser$Parser$symbol('r')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed('\\'),
			$elm$parser$Parser$symbol('\\')),
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						A2(
							$elm$core$Basics$composeR,
							$elm$core$String$toLower,
							A2(
								$elm$core$Basics$composeR,
								$rtfeldman$elm_hex$Hex$fromString,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Result$withDefault(0),
									$elm$core$Char$fromCode)))),
					$elm$parser$Parser$symbol('u')),
				$elm$parser$Parser$symbol('{')),
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$chompWhile(
						function (c) {
							return A2(
								$elm$core$String$any,
								$elm$core$Basics$eq(c),
								'0123456789ABCDEFabcdef');
						})),
				$elm$parser$Parser$symbol('}')))
		]));
var $stil4m$elm_syntax$Elm$Parser$Tokens$quotedSingleQuote = $stil4m$elm_syntax$Combine$fromCore(
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(
				A2(
					$elm$core$Basics$composeR,
					$elm$core$String$toList,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$List$head,
						$elm$core$Maybe$withDefault(' ')))),
			$elm$parser$Parser$symbol('\'')),
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed(
								A2($elm$core$Basics$composeR, $elm$core$List$singleton, $elm$core$String$fromList)),
							$elm$parser$Parser$symbol('\\')),
						$stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValue),
						$elm$parser$Parser$getChompedString(
						$elm$parser$Parser$chompIf(
							$elm$core$Basics$always(true)))
					])),
			$elm$parser$Parser$symbol('\''))));
var $stil4m$elm_syntax$Elm$Parser$Tokens$characterLiteral = A2(
	$stil4m$elm_syntax$Combine$or,
	$stil4m$elm_syntax$Elm$Parser$Tokens$quotedSingleQuote,
	A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Combine$Char$char('\''),
		A2(
			$stil4m$elm_syntax$Combine$continueWith,
			$stil4m$elm_syntax$Combine$Char$anyChar,
			$stil4m$elm_syntax$Combine$Char$char('\''))));
var $stil4m$elm_syntax$Elm$Parser$Node$asPointerLocation = function (_v0) {
	var line = _v0.aJ;
	var column = _v0.ec;
	return {ec: column, fD: line};
};
var $stil4m$elm_syntax$Combine$app = function (_v0) {
	var inner = _v0;
	return inner;
};
var $elm$parser$Parser$Advanced$getPosition = function (s) {
	return A3(
		$elm$parser$Parser$Advanced$Good,
		false,
		_Utils_Tuple2(s.fD, s.eb),
		s);
};
var $elm$parser$Parser$getPosition = $elm$parser$Parser$Advanced$getPosition;
var $stil4m$elm_syntax$Combine$withLocation = function (f) {
	return function (state) {
		return A2(
			$elm$parser$Parser$andThen,
			function (loc) {
				return A2(
					$stil4m$elm_syntax$Combine$app,
					f(loc),
					state);
			},
			A2(
				$elm$parser$Parser$map,
				function (_v0) {
					var row = _v0.a;
					var col = _v0.b;
					return {ec: col, aJ: row};
				},
				$elm$parser$Parser$getPosition));
	};
};
var $stil4m$elm_syntax$Elm$Parser$Node$parser = function (p) {
	return $stil4m$elm_syntax$Combine$withLocation(
		function (start) {
			return A2(
				$stil4m$elm_syntax$Combine$andMap,
				$stil4m$elm_syntax$Combine$withLocation(
					function (end) {
						return $stil4m$elm_syntax$Combine$succeed(
							{
								eq: $stil4m$elm_syntax$Elm$Parser$Node$asPointerLocation(end),
								fT: $stil4m$elm_syntax$Elm$Parser$Node$asPointerLocation(start)
							});
					}),
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					p,
					$stil4m$elm_syntax$Combine$succeed(
						F2(
							function (v, r) {
								return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, v);
							}))));
		});
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$charLiteralExpression = $stil4m$elm_syntax$Elm$Parser$Node$parser(
	A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Expression$CharLiteral, $stil4m$elm_syntax$Elm$Parser$Tokens$characterLiteral));
var $stil4m$elm_syntax$Elm$Parser$Tokens$elseToken = $stil4m$elm_syntax$Combine$string('else');
var $stil4m$elm_syntax$Elm$Parser$State$currentIndent = function (_v0) {
	var indents = _v0.ap;
	return A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$List$head(indents));
};
var $stil4m$elm_syntax$Elm$Parser$State$expectedColumn = A2(
	$elm$core$Basics$composeR,
	$stil4m$elm_syntax$Elm$Parser$State$currentIndent,
	$elm$core$Basics$add(1));
var $stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern = {$: 0};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$AsPattern = F2(
	function (a, b) {
		return {$: 13, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Pattern$CharPattern = function (a) {
	return {$: 2, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern = function (a) {
	return {$: 10, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern = function (a) {
	return {$: 14, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$QualifiedNameRef = F2(
	function (moduleName, name) {
		return {bs: moduleName, cN: name};
	});
var $stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern = function (a) {
	return {$: 7, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$UnConsPattern = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Pattern$UnitPattern = {$: 1};
var $stil4m$elm_syntax$Combine$between = F3(
	function (lp, rp, p) {
		return A2(
			$stil4m$elm_syntax$Combine$ignore,
			rp,
			A2($stil4m$elm_syntax$Combine$continueWith, p, lp));
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$reservedList = _List_fromArray(
	['module', 'exposing', 'import', 'as', 'if', 'then', 'else', 'let', 'in', 'case', 'of', 'port', 'infixr', 'infixl', 'type', 'where']);
var $elm$parser$Parser$ExpectingVariable = {$: 7};
var $elm$parser$Parser$Advanced$varHelp = F7(
	function (isGood, offset, row, col, src, indent, context) {
		varHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, src);
			if (_Utils_eq(newOffset, -1)) {
				return {eb: col, f: context, j: indent, b: offset, fD: row, a: src};
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$variable = function (i) {
	return function (s) {
		var firstOffset = A3($elm$parser$Parser$Advanced$isSubChar, i.fT, s.b, s.a);
		if (_Utils_eq(firstOffset, -1)) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.cl));
		} else {
			var s1 = _Utils_eq(firstOffset, -2) ? A7($elm$parser$Parser$Advanced$varHelp, i.cC, s.b + 1, s.fD + 1, 1, s.a, s.j, s.f) : A7($elm$parser$Parser$Advanced$varHelp, i.cC, firstOffset, s.fD, s.eb + 1, s.a, s.j, s.f);
			var name = A3($elm$core$String$slice, s.b, s1.b, s.a);
			return A2($elm$core$Set$member, name, i.c4) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.cl)) : A3($elm$parser$Parser$Advanced$Good, true, name, s1);
		}
	};
};
var $elm$parser$Parser$variable = function (i) {
	return $elm$parser$Parser$Advanced$variable(
		{cl: $elm$parser$Parser$ExpectingVariable, cC: i.cC, c4: i.c4, fT: i.fT});
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$functionName = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$variable(
		{
			cC: function (c) {
				return $elm$core$Char$isAlphaNum(c) || (c === '_');
			},
			c4: $elm$core$Set$fromList($stil4m$elm_syntax$Elm$Parser$Tokens$reservedList),
			fT: $elm$core$Char$isLower
		}));
var $elm$parser$Parser$ExpectingKeyword = function (a) {
	return {$: 9, a: a};
};
var $elm$parser$Parser$Advanced$keyword = function (_v0) {
	var kwd = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(kwd);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, kwd, s.b, s.fD, s.eb, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return (_Utils_eq(newOffset, -1) || (0 <= A3(
			$elm$parser$Parser$Advanced$isSubChar,
			function (c) {
				return $elm$core$Char$isAlphaNum(c) || (c === '_');
			},
			newOffset,
			s.a))) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{eb: newCol, f: s.f, j: s.j, b: newOffset, fD: newRow, a: s.a});
	};
};
var $elm$parser$Parser$keyword = function (kwd) {
	return $elm$parser$Parser$Advanced$keyword(
		A2(
			$elm$parser$Parser$Advanced$Token,
			kwd,
			$elm$parser$Parser$ExpectingKeyword(kwd)));
};
var $elm$parser$Parser$Advanced$lazy = function (thunk) {
	return function (s) {
		var _v0 = thunk(0);
		var parse = _v0;
		return parse(s);
	};
};
var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
var $stil4m$elm_syntax$Combine$lazy = function (t) {
	return function (state) {
		return $elm$parser$Parser$lazy(
			function (_v0) {
				return function (_v1) {
					var t_ = _v1;
					return t_(state);
				}(
					t(0));
			});
	};
};
var $elm$parser$Parser$Nestable = 1;
var $elm$parser$Parser$Advanced$findSubString = _Parser_findSubString;
var $elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var $elm$parser$Parser$Advanced$chompUntil = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$findSubString, str, s.b, s.fD, s.eb, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A4($elm$parser$Parser$Advanced$fromInfo, newRow, newCol, expecting, s.f)) : A3(
			$elm$parser$Parser$Advanced$Good,
			_Utils_cmp(s.b, newOffset) < 0,
			0,
			{eb: newCol, f: s.f, j: s.j, b: newOffset, fD: newRow, a: s.a});
	};
};
var $elm$parser$Parser$Advanced$isChar = function (_char) {
	return true;
};
var $elm$parser$Parser$Advanced$revAlways = F2(
	function (_v0, b) {
		return b;
	});
var $elm$parser$Parser$Advanced$skip = F2(
	function (iParser, kParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$parser$Parser$Advanced$revAlways, iParser, kParser);
	});
var $elm$parser$Parser$Advanced$nestableHelp = F5(
	function (isNotRelevant, open, close, expectingClose, nestLevel) {
		return A2(
			$elm$parser$Parser$Advanced$skip,
			$elm$parser$Parser$Advanced$chompWhile(isNotRelevant),
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						(nestLevel === 1) ? close : A2(
						$elm$parser$Parser$Advanced$andThen,
						function (_v0) {
							return A5($elm$parser$Parser$Advanced$nestableHelp, isNotRelevant, open, close, expectingClose, nestLevel - 1);
						},
						close),
						A2(
						$elm$parser$Parser$Advanced$andThen,
						function (_v1) {
							return A5($elm$parser$Parser$Advanced$nestableHelp, isNotRelevant, open, close, expectingClose, nestLevel + 1);
						},
						open),
						A2(
						$elm$parser$Parser$Advanced$andThen,
						function (_v2) {
							return A5($elm$parser$Parser$Advanced$nestableHelp, isNotRelevant, open, close, expectingClose, nestLevel);
						},
						A2($elm$parser$Parser$Advanced$chompIf, $elm$parser$Parser$Advanced$isChar, expectingClose))
					])));
	});
var $elm$parser$Parser$Advanced$nestableComment = F2(
	function (open, close) {
		var oStr = open.a;
		var oX = open.b;
		var cStr = close.a;
		var cX = close.b;
		var _v0 = $elm$core$String$uncons(oStr);
		if (_v0.$ === 1) {
			return $elm$parser$Parser$Advanced$problem(oX);
		} else {
			var _v1 = _v0.a;
			var openChar = _v1.a;
			var _v2 = $elm$core$String$uncons(cStr);
			if (_v2.$ === 1) {
				return $elm$parser$Parser$Advanced$problem(cX);
			} else {
				var _v3 = _v2.a;
				var closeChar = _v3.a;
				var isNotRelevant = function (_char) {
					return (!_Utils_eq(_char, openChar)) && (!_Utils_eq(_char, closeChar));
				};
				var chompOpen = $elm$parser$Parser$Advanced$token(open);
				return A2(
					$elm$parser$Parser$Advanced$ignorer,
					chompOpen,
					A5(
						$elm$parser$Parser$Advanced$nestableHelp,
						isNotRelevant,
						chompOpen,
						$elm$parser$Parser$Advanced$token(close),
						cX,
						1));
			}
		}
	});
var $elm$parser$Parser$Advanced$multiComment = F3(
	function (open, close, nestable) {
		if (!nestable) {
			return A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$token(open),
				$elm$parser$Parser$Advanced$chompUntil(close));
		} else {
			return A2($elm$parser$Parser$Advanced$nestableComment, open, close);
		}
	});
var $elm$parser$Parser$Advanced$Nestable = 1;
var $elm$parser$Parser$Advanced$NotNestable = 0;
var $elm$parser$Parser$toAdvancedNestable = function (nestable) {
	if (!nestable) {
		return 0;
	} else {
		return 1;
	}
};
var $elm$parser$Parser$multiComment = F3(
	function (open, close, nestable) {
		return A3(
			$elm$parser$Parser$Advanced$multiComment,
			$elm$parser$Parser$toToken(open),
			$elm$parser$Parser$toToken(close),
			$elm$parser$Parser$toAdvancedNestable(nestable));
	});
var $stil4m$elm_syntax$Elm$Parser$Comments$multilineCommentInner = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$getChompedString(
		A3($elm$parser$Parser$multiComment, '{-', '-}', 1)));
var $stil4m$elm_syntax$Elm$Parser$State$addComment = F2(
	function (pair, _v0) {
		var s = _v0;
		return _Utils_update(
			s,
			{
				aX: A2($elm$core$List$cons, pair, s.aX)
			});
	});
var $stil4m$elm_syntax$Combine$modifyState = function (f) {
	return function (state) {
		return $elm$parser$Parser$succeed(
			_Utils_Tuple2(
				f(state),
				0));
	};
};
var $stil4m$elm_syntax$Elm$Parser$Comments$addCommentToState = function (p) {
	return A2(
		$stil4m$elm_syntax$Combine$andThen,
		function (pair) {
			return A2(
				$stil4m$elm_syntax$Combine$continueWith,
				$stil4m$elm_syntax$Combine$succeed(0),
				$stil4m$elm_syntax$Combine$modifyState(
					$stil4m$elm_syntax$Elm$Parser$State$addComment(pair)));
		},
		p);
};
var $stil4m$elm_syntax$Elm$Parser$Comments$parseComment = function (commentParser) {
	return $stil4m$elm_syntax$Elm$Parser$Comments$addCommentToState(
		$stil4m$elm_syntax$Elm$Parser$Node$parser(commentParser));
};
var $stil4m$elm_syntax$Elm$Parser$Comments$multilineComment = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Elm$Parser$Comments$parseComment($stil4m$elm_syntax$Elm$Parser$Comments$multilineCommentInner);
	});
var $stil4m$elm_syntax$Elm$Parser$Whitespace$untilNewlineToken = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompWhile(
			function (c) {
				return (c !== '\u000D') && (c !== '\n');
			})));
var $stil4m$elm_syntax$Elm$Parser$Comments$singleLineComment = $stil4m$elm_syntax$Elm$Parser$Comments$parseComment(
	A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$Whitespace$untilNewlineToken,
		A2(
			$stil4m$elm_syntax$Combine$andMap,
			$stil4m$elm_syntax$Combine$string('--'),
			$stil4m$elm_syntax$Combine$succeed($elm$core$Basics$append))));
var $stil4m$elm_syntax$Elm$Parser$Layout$anyComment = A2($stil4m$elm_syntax$Combine$or, $stil4m$elm_syntax$Elm$Parser$Comments$singleLineComment, $stil4m$elm_syntax$Elm$Parser$Comments$multilineComment);
var $elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0;
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (!step.$) {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (!step.$) {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $stil4m$elm_syntax$Combine$many = function (p) {
	var helper = function (_v2) {
		var oldState = _v2.a;
		var items = _v2.b;
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed(
						function (_v0) {
							var newState = _v0.a;
							var item = _v0.b;
							return $elm$parser$Parser$Loop(
								_Utils_Tuple2(
									newState,
									A2($elm$core$List$cons, item, items)));
						}),
					A2($stil4m$elm_syntax$Combine$app, p, oldState)),
					A2(
					$elm$parser$Parser$map,
					function (_v1) {
						return $elm$parser$Parser$Done(
							_Utils_Tuple2(
								oldState,
								$elm$core$List$reverse(items)));
					},
					$elm$parser$Parser$succeed(0))
				]));
	};
	return function (state) {
		return A2(
			$elm$parser$Parser$loop,
			_Utils_Tuple2(state, _List_Nil),
			helper);
	};
};
var $stil4m$elm_syntax$Combine$many1 = function (p) {
	return A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Combine$many(p),
		A2(
			$stil4m$elm_syntax$Combine$andMap,
			p,
			$stil4m$elm_syntax$Combine$succeed($elm$core$List$cons)));
};
var $stil4m$elm_syntax$Elm$Parser$Whitespace$many1Spaces = $stil4m$elm_syntax$Combine$fromCore(
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$token(' '),
		$elm$parser$Parser$chompWhile(
			function (c) {
				return c === ' ';
			})));
var $stil4m$elm_syntax$Elm$Parser$Whitespace$realNewLine = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$getChompedString(
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(0),
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							$elm$parser$Parser$chompIf(
							$elm$core$Basics$eq('\u000D')),
							$elm$parser$Parser$succeed(0)
						]))),
			$elm$parser$Parser$symbol('\n'))));
var $stil4m$elm_syntax$Elm$Parser$Layout$verifyIndent = function (f) {
	return $stil4m$elm_syntax$Combine$withState(
		function (s) {
			return $stil4m$elm_syntax$Combine$withLocation(
				function (l) {
					return A2(
						f,
						$stil4m$elm_syntax$Elm$Parser$State$expectedColumn(s),
						l.ec) ? $stil4m$elm_syntax$Combine$succeed(0) : $stil4m$elm_syntax$Combine$fail(
						'Expected higher indent than ' + $elm$core$String$fromInt(l.ec));
				});
		});
};
var $stil4m$elm_syntax$Elm$Parser$Layout$layout = A2(
	$stil4m$elm_syntax$Combine$continueWith,
	$stil4m$elm_syntax$Elm$Parser$Layout$verifyIndent(
		F2(
			function (stateIndent, current) {
				return _Utils_cmp(stateIndent, current) < 0;
			})),
	$stil4m$elm_syntax$Combine$many1(
		$stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					$stil4m$elm_syntax$Elm$Parser$Layout$anyComment,
					A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[$stil4m$elm_syntax$Elm$Parser$Whitespace$many1Spaces, $stil4m$elm_syntax$Elm$Parser$Layout$anyComment])),
					$stil4m$elm_syntax$Combine$many1($stil4m$elm_syntax$Elm$Parser$Whitespace$realNewLine)),
					$stil4m$elm_syntax$Elm$Parser$Whitespace$many1Spaces
				]))));
var $stil4m$elm_syntax$Combine$maybe = function (_v0) {
	var p = _v0;
	return function (state) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v1) {
						var c = _v1.a;
						var v = _v1.b;
						return _Utils_Tuple2(
							c,
							$elm$core$Maybe$Just(v));
					},
					p(state)),
					$elm$parser$Parser$succeed(
					_Utils_Tuple2(state, $elm$core$Maybe$Nothing))
				]));
	};
};
var $stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides = function (x) {
	return A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
		A2(
			$stil4m$elm_syntax$Combine$continueWith,
			x,
			$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout)));
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$FloatPattern = function (a) {
	return {$: 6, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$HexPattern = function (a) {
	return {$: 5, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$IntPattern = function (a) {
	return {$: 4, a: a};
};
var $elm$parser$Parser$ExpectingBinary = {$: 4};
var $elm$parser$Parser$ExpectingFloat = {$: 5};
var $elm$parser$Parser$ExpectingHex = {$: 2};
var $elm$parser$Parser$ExpectingInt = {$: 1};
var $elm$parser$Parser$ExpectingNumber = {$: 6};
var $elm$parser$Parser$ExpectingOctal = {$: 3};
var $elm$parser$Parser$Advanced$consumeBase = _Parser_consumeBase;
var $elm$parser$Parser$Advanced$consumeBase16 = _Parser_consumeBase16;
var $elm$parser$Parser$Advanced$bumpOffset = F2(
	function (newOffset, s) {
		return {eb: s.eb + (newOffset - s.b), f: s.f, j: s.j, b: newOffset, fD: s.fD, a: s.a};
	});
var $elm$parser$Parser$Advanced$chompBase10 = _Parser_chompBase10;
var $elm$parser$Parser$Advanced$isAsciiCode = _Parser_isAsciiCode;
var $elm$parser$Parser$Advanced$consumeExp = F2(
	function (offset, src) {
		if (A3($elm$parser$Parser$Advanced$isAsciiCode, 101, offset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 69, offset, src)) {
			var eOffset = offset + 1;
			var expOffset = (A3($elm$parser$Parser$Advanced$isAsciiCode, 43, eOffset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 45, eOffset, src)) ? (eOffset + 1) : eOffset;
			var newOffset = A2($elm$parser$Parser$Advanced$chompBase10, expOffset, src);
			return _Utils_eq(expOffset, newOffset) ? (-newOffset) : newOffset;
		} else {
			return offset;
		}
	});
var $elm$parser$Parser$Advanced$consumeDotAndExp = F2(
	function (offset, src) {
		return A3($elm$parser$Parser$Advanced$isAsciiCode, 46, offset, src) ? A2(
			$elm$parser$Parser$Advanced$consumeExp,
			A2($elm$parser$Parser$Advanced$chompBase10, offset + 1, src),
			src) : A2($elm$parser$Parser$Advanced$consumeExp, offset, src);
	});
var $elm$parser$Parser$Advanced$finalizeInt = F5(
	function (invalid, handler, startOffset, _v0, s) {
		var endOffset = _v0.a;
		var n = _v0.b;
		if (handler.$ === 1) {
			var x = handler.a;
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		} else {
			var toValue = handler.a;
			return _Utils_eq(startOffset, endOffset) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				_Utils_cmp(s.b, startOffset) < 0,
				A2($elm$parser$Parser$Advanced$fromState, s, invalid)) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				toValue(n),
				A2($elm$parser$Parser$Advanced$bumpOffset, endOffset, s));
		}
	});
var $elm$core$String$toFloat = _String_toFloat;
var $elm$parser$Parser$Advanced$finalizeFloat = F6(
	function (invalid, expecting, intSettings, floatSettings, intPair, s) {
		var intOffset = intPair.a;
		var floatOffset = A2($elm$parser$Parser$Advanced$consumeDotAndExp, intOffset, s.a);
		if (floatOffset < 0) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A4($elm$parser$Parser$Advanced$fromInfo, s.fD, s.eb - (floatOffset + s.b), invalid, s.f));
		} else {
			if (_Utils_eq(s.b, floatOffset)) {
				return A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting));
			} else {
				if (_Utils_eq(intOffset, floatOffset)) {
					return A5($elm$parser$Parser$Advanced$finalizeInt, invalid, intSettings, s.b, intPair, s);
				} else {
					if (floatSettings.$ === 1) {
						var x = floatSettings.a;
						return A2(
							$elm$parser$Parser$Advanced$Bad,
							true,
							A2($elm$parser$Parser$Advanced$fromState, s, invalid));
					} else {
						var toValue = floatSettings.a;
						var _v1 = $elm$core$String$toFloat(
							A3($elm$core$String$slice, s.b, floatOffset, s.a));
						if (_v1.$ === 1) {
							return A2(
								$elm$parser$Parser$Advanced$Bad,
								true,
								A2($elm$parser$Parser$Advanced$fromState, s, invalid));
						} else {
							var n = _v1.a;
							return A3(
								$elm$parser$Parser$Advanced$Good,
								true,
								toValue(n),
								A2($elm$parser$Parser$Advanced$bumpOffset, floatOffset, s));
						}
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$number = function (c) {
	return function (s) {
		if (A3($elm$parser$Parser$Advanced$isAsciiCode, 48, s.b, s.a)) {
			var zeroOffset = s.b + 1;
			var baseOffset = zeroOffset + 1;
			return A3($elm$parser$Parser$Advanced$isAsciiCode, 120, zeroOffset, s.a) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.eW,
				c.cz,
				baseOffset,
				A2($elm$parser$Parser$Advanced$consumeBase16, baseOffset, s.a),
				s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 111, zeroOffset, s.a) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.eW,
				c.cQ,
				baseOffset,
				A3($elm$parser$Parser$Advanced$consumeBase, 8, baseOffset, s.a),
				s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 98, zeroOffset, s.a) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.eW,
				c.b6,
				baseOffset,
				A3($elm$parser$Parser$Advanced$consumeBase, 2, baseOffset, s.a),
				s) : A6(
				$elm$parser$Parser$Advanced$finalizeFloat,
				c.eW,
				c.cl,
				c.cE,
				c.cn,
				_Utils_Tuple2(zeroOffset, 0),
				s)));
		} else {
			return A6(
				$elm$parser$Parser$Advanced$finalizeFloat,
				c.eW,
				c.cl,
				c.cE,
				c.cn,
				A3($elm$parser$Parser$Advanced$consumeBase, 10, s.b, s.a),
				s);
		}
	};
};
var $elm$parser$Parser$number = function (i) {
	return $elm$parser$Parser$Advanced$number(
		{
			b6: A2($elm$core$Result$fromMaybe, $elm$parser$Parser$ExpectingBinary, i.b6),
			cl: $elm$parser$Parser$ExpectingNumber,
			cn: A2($elm$core$Result$fromMaybe, $elm$parser$Parser$ExpectingFloat, i.cn),
			cz: A2($elm$core$Result$fromMaybe, $elm$parser$Parser$ExpectingHex, i.cz),
			cE: A2($elm$core$Result$fromMaybe, $elm$parser$Parser$ExpectingInt, i.cE),
			eW: $elm$parser$Parser$ExpectingNumber,
			cQ: A2($elm$core$Result$fromMaybe, $elm$parser$Parser$ExpectingOctal, i.cQ)
		});
};
var $stil4m$elm_syntax$Elm$Parser$Numbers$raw = F3(
	function (floatf, intf, hexf) {
		return $elm$parser$Parser$number(
			{
				b6: $elm$core$Maybe$Nothing,
				cn: $elm$core$Maybe$Just(floatf),
				cz: $elm$core$Maybe$Just(hexf),
				cE: $elm$core$Maybe$Just(intf),
				cQ: $elm$core$Maybe$Nothing
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Numbers$number = F3(
	function (floatf, intf, hexf) {
		return $stil4m$elm_syntax$Combine$fromCore(
			A3($stil4m$elm_syntax$Elm$Parser$Numbers$raw, floatf, intf, hexf));
	});
var $stil4m$elm_syntax$Elm$Parser$Patterns$numberPart = A3($stil4m$elm_syntax$Elm$Parser$Numbers$number, $stil4m$elm_syntax$Elm$Syntax$Pattern$FloatPattern, $stil4m$elm_syntax$Elm$Syntax$Pattern$IntPattern, $stil4m$elm_syntax$Elm$Syntax$Pattern$HexPattern);
var $stil4m$elm_syntax$Combine$parens = A2(
	$stil4m$elm_syntax$Combine$between,
	$stil4m$elm_syntax$Combine$string('('),
	$stil4m$elm_syntax$Combine$string(')'));
var $stil4m$elm_syntax$Elm$Syntax$Pattern$RecordPattern = function (a) {
	return {$: 8, a: a};
};
var $stil4m$elm_syntax$Combine$sepBy1 = F2(
	function (sep, p) {
		return A2(
			$stil4m$elm_syntax$Combine$andMap,
			$stil4m$elm_syntax$Combine$many(
				A2($stil4m$elm_syntax$Combine$continueWith, p, sep)),
			A2(
				$stil4m$elm_syntax$Combine$andMap,
				p,
				$stil4m$elm_syntax$Combine$succeed($elm$core$List$cons)));
	});
var $stil4m$elm_syntax$Elm$Parser$Patterns$recordPart = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Elm$Parser$Node$parser(
			A2(
				$stil4m$elm_syntax$Combine$map,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$RecordPattern,
				A3(
					$stil4m$elm_syntax$Combine$between,
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						$stil4m$elm_syntax$Combine$string('{')),
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Combine$string('}'),
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout)),
					A2(
						$stil4m$elm_syntax$Combine$sepBy1,
						$stil4m$elm_syntax$Combine$string(','),
						$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
							$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName))))));
	});
var $stil4m$elm_syntax$Combine$sepBy = F2(
	function (sep, p) {
		return A2(
			$stil4m$elm_syntax$Combine$or,
			A2($stil4m$elm_syntax$Combine$sepBy1, sep, p),
			$stil4m$elm_syntax$Combine$succeed(_List_Nil));
	});
var $elm$parser$Parser$Advanced$getOffset = function (s) {
	return A3($elm$parser$Parser$Advanced$Good, false, s.b, s);
};
var $elm$parser$Parser$getOffset = $elm$parser$Parser$Advanced$getOffset;
var $stil4m$elm_syntax$Elm$Parser$Tokens$stringLiteral = function () {
	var helper = function (s) {
		return s.Q ? A2(
			$elm$parser$Parser$map,
			function (v) {
				return $elm$parser$Parser$Loop(
					_Utils_update(
						s,
						{
							Q: false,
							p: A2(
								$elm$core$List$cons,
								$elm$core$String$fromList(
									_List_fromArray(
										[v])),
								s.p)
						}));
			},
			$stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValue) : $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return $elm$parser$Parser$Done(
							$elm$core$String$concat(
								$elm$core$List$reverse(s.p)));
					},
					$elm$parser$Parser$symbol('\"')),
					A2(
					$elm$parser$Parser$map,
					function (_v1) {
						return $elm$parser$Parser$Loop(
							_Utils_update(
								s,
								{Q: true, p: s.p}));
					},
					$elm$parser$Parser$getChompedString(
						$elm$parser$Parser$symbol('\\'))),
					A2(
					$elm$parser$Parser$andThen,
					function (_v2) {
						var start = _v2.a;
						var value = _v2.b;
						var end = _v2.c;
						return _Utils_eq(start, end) ? $elm$parser$Parser$problem('Expected a string character or a double quote') : $elm$parser$Parser$succeed(
							$elm$parser$Parser$Loop(
								_Utils_update(
									s,
									{
										p: A2($elm$core$List$cons, value, s.p)
									})));
					},
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								$elm$parser$Parser$succeed(
									F3(
										function (start, value, end) {
											return _Utils_Tuple3(start, value, end);
										})),
								$elm$parser$Parser$getOffset),
							$elm$parser$Parser$getChompedString(
								$elm$parser$Parser$chompWhile(
									function (c) {
										return (c !== '\"') && (c !== '\\');
									}))),
						$elm$parser$Parser$getOffset))
				]));
	};
	return $stil4m$elm_syntax$Combine$fromCore(
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$symbol('\"')),
			A2(
				$elm$parser$Parser$loop,
				{Q: false, p: _List_Nil},
				helper)));
}();
var $stil4m$elm_syntax$Elm$Parser$Tokens$typeName = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$variable(
		{
			cC: function (c) {
				return $elm$core$Char$isAlphaNum(c) || (c === '_');
			},
			c4: $elm$core$Set$fromList($stil4m$elm_syntax$Elm$Parser$Tokens$reservedList),
			fT: $elm$core$Char$isUpper
		}));
var $stil4m$elm_syntax$Elm$Parser$Base$typeIndicator = function () {
	var helper = function (_v0) {
		var n = _v0.a;
		var xs = _v0.b;
		return $stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Combine$andThen,
					function (t) {
						return helper(
							_Utils_Tuple2(
								t,
								A2($elm$core$List$cons, n, xs)));
					},
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
						$stil4m$elm_syntax$Combine$string('.'))),
					$stil4m$elm_syntax$Combine$succeed(
					_Utils_Tuple2(n, xs))
				]));
	};
	return A2(
		$stil4m$elm_syntax$Combine$map,
		function (_v1) {
			var t = _v1.a;
			var xs = _v1.b;
			return _Utils_Tuple2(
				$elm$core$List$reverse(xs),
				t);
		},
		A2(
			$stil4m$elm_syntax$Combine$andThen,
			function (t) {
				return helper(
					_Utils_Tuple2(t, _List_Nil));
			},
			$stil4m$elm_syntax$Elm$Parser$Tokens$typeName));
}();
var $stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern = function (a) {
	return {$: 11, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$variablePart = $stil4m$elm_syntax$Elm$Parser$Node$parser(
	A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern, $stil4m$elm_syntax$Elm$Parser$Tokens$functionName));
var $stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPattern = function (consumeArgs) {
	return A2(
		$stil4m$elm_syntax$Combine$andThen,
		function (_v0) {
			var range = _v0.a;
			var _v1 = _v0.b;
			var mod = _v1.a;
			var name = _v1.b;
			return A2(
				$stil4m$elm_syntax$Combine$map,
				function (args) {
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						$stil4m$elm_syntax$Elm$Syntax$Range$combine(
							A2(
								$elm$core$List$cons,
								range,
								A2(
									$elm$core$List$map,
									function (_v2) {
										var r = _v2.a;
										return r;
									},
									args))),
						A2(
							$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
							A2($stil4m$elm_syntax$Elm$Syntax$Pattern$QualifiedNameRef, mod, name),
							args));
				},
				consumeArgs ? $stil4m$elm_syntax$Combine$many(
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternArg())) : $stil4m$elm_syntax$Combine$succeed(_List_Nil));
		},
		A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
			$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Base$typeIndicator)));
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$tryToCompose = function (x) {
	return A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Combine$map,
					function (y) {
						return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Pattern$AsPattern, x, y);
					},
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Elm$Parser$Layout$layout,
							$stil4m$elm_syntax$Combine$fromCore(
								$elm$parser$Parser$keyword('as'))))),
					A2(
					$stil4m$elm_syntax$Combine$map,
					function (y) {
						return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Pattern$UnConsPattern, x, y);
					},
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern(),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							$stil4m$elm_syntax$Combine$fromCore(
								$elm$parser$Parser$symbol('::'))))),
					$stil4m$elm_syntax$Combine$succeed(x)
				])),
		$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout));
};
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern() {
	return A2(
		$stil4m$elm_syntax$Combine$andThen,
		$stil4m$elm_syntax$Elm$Parser$Patterns$tryToCompose,
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern());
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern() {
	return $stil4m$elm_syntax$Combine$choice(
		_List_fromArray(
			[
				$stil4m$elm_syntax$Elm$Parser$Patterns$variablePart,
				$stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPattern(true),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern, $stil4m$elm_syntax$Elm$Parser$Tokens$stringLiteral)),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Pattern$CharPattern, $stil4m$elm_syntax$Elm$Parser$Tokens$characterLiteral)),
				$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Patterns$numberPart),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$map,
					$elm$core$Basics$always($stil4m$elm_syntax$Elm$Syntax$Pattern$UnitPattern),
					$stil4m$elm_syntax$Combine$fromCore(
						$elm$parser$Parser$symbol('()')))),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$map,
					$elm$core$Basics$always($stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern),
					$stil4m$elm_syntax$Combine$fromCore(
						$elm$parser$Parser$symbol('_')))),
				$stil4m$elm_syntax$Elm$Parser$Patterns$recordPart,
				$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern(),
				$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern()
			]));
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternArg() {
	return $stil4m$elm_syntax$Combine$choice(
		_List_fromArray(
			[
				$stil4m$elm_syntax$Elm$Parser$Patterns$variablePart,
				$stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPattern(false),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern, $stil4m$elm_syntax$Elm$Parser$Tokens$stringLiteral)),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Pattern$CharPattern, $stil4m$elm_syntax$Elm$Parser$Tokens$characterLiteral)),
				$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Patterns$numberPart),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$map,
					$elm$core$Basics$always($stil4m$elm_syntax$Elm$Syntax$Pattern$UnitPattern),
					$stil4m$elm_syntax$Combine$fromCore(
						$elm$parser$Parser$symbol('()')))),
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$map,
					$elm$core$Basics$always($stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern),
					$stil4m$elm_syntax$Combine$fromCore(
						$elm$parser$Parser$symbol('_')))),
				$stil4m$elm_syntax$Elm$Parser$Patterns$recordPart,
				$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern(),
				$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern()
			]));
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v5) {
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A3(
					$stil4m$elm_syntax$Combine$between,
					$stil4m$elm_syntax$Combine$string('['),
					$stil4m$elm_syntax$Combine$string(']'),
					A2(
						$stil4m$elm_syntax$Combine$map,
						$stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern,
						A2(
							$stil4m$elm_syntax$Combine$sepBy,
							$stil4m$elm_syntax$Combine$string(','),
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
								$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern())))));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v3) {
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$map,
					function (c) {
						if (c.b && (!c.b.b)) {
							var x = c.a;
							return $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern(x);
						} else {
							return $stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(c);
						}
					},
					$stil4m$elm_syntax$Combine$parens(
						A2(
							$stil4m$elm_syntax$Combine$sepBy,
							$stil4m$elm_syntax$Combine$string(','),
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
								$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern())))));
		});
}
var $stil4m$elm_syntax$Elm$Parser$Patterns$pattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern();
$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern = function () {
	return $stil4m$elm_syntax$Elm$Parser$Patterns$pattern;
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$composablePattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern();
$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern = function () {
	return $stil4m$elm_syntax$Elm$Parser$Patterns$composablePattern;
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPatternArg = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternArg();
$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternArg = function () {
	return $stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPatternArg;
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$listPattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern();
$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern = function () {
	return $stil4m$elm_syntax$Elm$Parser$Patterns$listPattern;
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$parensPattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern();
$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern = function () {
	return $stil4m$elm_syntax$Elm$Parser$Patterns$parensPattern;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$functionArgument = $stil4m$elm_syntax$Elm$Parser$Patterns$pattern;
var $stil4m$elm_syntax$Elm$Syntax$Signature$Signature = F2(
	function (name, typeAnnotation) {
		return {cN: name, dn: typeAnnotation};
	});
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$Eager = 0;
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$Lazy = 1;
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record = function (a) {
	return {$: 4, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit = {$: 2};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$asTypeAnnotation = F2(
	function (x, xs) {
		var value = x.b;
		if (!xs.b) {
			return value;
		} else {
			return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
				A2($elm$core$List$cons, x, xs));
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$genericTypeAnnotation = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Elm$Parser$Node$parser(
			A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType, $stil4m$elm_syntax$Elm$Parser$Tokens$functionName));
	});
var $stil4m$elm_syntax$Elm$Parser$Layout$Indented = 1;
var $stil4m$elm_syntax$Elm$Parser$Layout$Strict = 0;
var $stil4m$elm_syntax$Elm$Parser$State$storedColumns = function (_v0) {
	var indents = _v0.ap;
	return A2(
		$elm$core$List$map,
		$elm$core$Basics$add(1),
		indents);
};
var $stil4m$elm_syntax$Elm$Parser$Layout$compute = $stil4m$elm_syntax$Combine$withState(
	function (s) {
		return $stil4m$elm_syntax$Combine$withLocation(
			function (l) {
				var known = A2(
					$elm$core$List$cons,
					1,
					$stil4m$elm_syntax$Elm$Parser$State$storedColumns(s));
				return A2($elm$core$List$member, l.ec, known) ? $stil4m$elm_syntax$Combine$succeed(0) : $stil4m$elm_syntax$Combine$succeed(1);
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout = A2(
	$stil4m$elm_syntax$Combine$continueWith,
	$stil4m$elm_syntax$Elm$Parser$Layout$compute,
	$stil4m$elm_syntax$Combine$many(
		$stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					$stil4m$elm_syntax$Elm$Parser$Layout$anyComment,
					A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								$stil4m$elm_syntax$Elm$Parser$Whitespace$many1Spaces,
								$stil4m$elm_syntax$Elm$Parser$Layout$anyComment,
								$stil4m$elm_syntax$Combine$succeed(0)
							])),
					$stil4m$elm_syntax$Combine$many1($stil4m$elm_syntax$Elm$Parser$Whitespace$realNewLine)),
					$stil4m$elm_syntax$Elm$Parser$Whitespace$many1Spaces
				]))));
var $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayoutWith = F2(
	function (onStrict, onIndented) {
		return A2(
			$stil4m$elm_syntax$Combine$andThen,
			function (ind) {
				if (!ind) {
					return onStrict(0);
				} else {
					return onIndented(0);
				}
			},
			$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout);
	});
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFn = function (mode) {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v7) {
			return $stil4m$elm_syntax$Combine$choice(
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation(),
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typedTypeAnnotation(mode),
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$genericTypeAnnotation,
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation()
					]));
		});
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typedTypeAnnotation = function (mode) {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v0) {
			var nodeRanges = $elm$core$List$map(
				function (_v6) {
					var r = _v6.a;
					return r;
				});
			var genericHelper = function (items) {
				return A2(
					$stil4m$elm_syntax$Combine$or,
					A2(
						$stil4m$elm_syntax$Combine$andThen,
						function (next) {
							return A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
								A2(
									$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayoutWith,
									function (_v1) {
										return $stil4m$elm_syntax$Combine$succeed(
											$elm$core$List$reverse(
												A2($elm$core$List$cons, next, items)));
									},
									function (_v2) {
										return genericHelper(
											A2($elm$core$List$cons, next, items));
									}));
						},
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFn(1)),
					$stil4m$elm_syntax$Combine$succeed(
						$elm$core$List$reverse(items)));
			};
			return A2(
				$stil4m$elm_syntax$Combine$andThen,
				function (original) {
					var tir = original.a;
					return A2(
						$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayoutWith,
						function (_v3) {
							return $stil4m$elm_syntax$Combine$succeed(
								A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									tir,
									A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, original, _List_Nil)));
						},
						function (_v4) {
							if (!mode) {
								return A2(
									$stil4m$elm_syntax$Combine$map,
									function (args) {
										return A2(
											$stil4m$elm_syntax$Elm$Syntax$Node$Node,
											$stil4m$elm_syntax$Elm$Syntax$Range$combine(
												A2(
													$elm$core$List$cons,
													tir,
													nodeRanges(args))),
											A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, original, args));
									},
									genericHelper(_List_Nil));
							} else {
								return $stil4m$elm_syntax$Combine$succeed(
									A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										tir,
										A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, original, _List_Nil)));
							}
						});
				},
				$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Base$typeIndicator));
		});
};
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v14) {
			var commaSep = $stil4m$elm_syntax$Combine$many(
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							$stil4m$elm_syntax$Combine$string(',')))));
			var nested = A2(
				$stil4m$elm_syntax$Combine$andMap,
				commaSep,
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Parser$TypeAnnotation$asTypeAnnotation)))));
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								A2(
								$stil4m$elm_syntax$Combine$map,
								$elm$core$Basics$always($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit),
								$stil4m$elm_syntax$Combine$string(')')),
								A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$string(')'),
								nested)
							])),
					$stil4m$elm_syntax$Combine$string('(')));
		});
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v13) {
			return A2(
				$stil4m$elm_syntax$Combine$andMap,
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						A2(
							$stil4m$elm_syntax$Combine$continueWith,
							$stil4m$elm_syntax$Combine$string(':'),
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout)))),
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout)),
					$stil4m$elm_syntax$Combine$succeed($elm$core$Tuple$pair)));
		});
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v12) {
			return A2(
				$stil4m$elm_syntax$Combine$sepBy,
				$stil4m$elm_syntax$Combine$string(','),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
					$stil4m$elm_syntax$Elm$Parser$Node$parser(
						$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition())));
		});
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v11) {
			var nextField = A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$string(':'),
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
								A2(
									$stil4m$elm_syntax$Combine$andMap,
									$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
									A2(
										$stil4m$elm_syntax$Combine$ignore,
										$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
										A2(
											$stil4m$elm_syntax$Combine$ignore,
											$stil4m$elm_syntax$Combine$string(','),
											$stil4m$elm_syntax$Combine$succeed(
												F2(
													function (a, b) {
														return _Utils_Tuple2(a, b);
													}))))))))));
			var additionalRecordFields = function (items) {
				return $stil4m$elm_syntax$Combine$choice(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Combine$andThen,
							function (next) {
								return additionalRecordFields(
									A2($elm$core$List$cons, next, items));
							},
							$stil4m$elm_syntax$Elm$Parser$Node$parser(nextField)),
							$stil4m$elm_syntax$Combine$succeed(
							$elm$core$List$reverse(items))
						]));
			};
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								A2(
								$stil4m$elm_syntax$Combine$continueWith,
								$stil4m$elm_syntax$Combine$succeed(
									$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(_List_Nil)),
								$stil4m$elm_syntax$Combine$string('}')),
								A2(
								$stil4m$elm_syntax$Combine$andThen,
								function (fname) {
									return $stil4m$elm_syntax$Combine$choice(
										_List_fromArray(
											[
												A2(
												$stil4m$elm_syntax$Combine$ignore,
												$stil4m$elm_syntax$Combine$string('}'),
												A2(
													$stil4m$elm_syntax$Combine$andMap,
													$stil4m$elm_syntax$Elm$Parser$Node$parser(
														$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation()),
													A2(
														$stil4m$elm_syntax$Combine$ignore,
														$stil4m$elm_syntax$Combine$string('|'),
														$stil4m$elm_syntax$Combine$succeed(
															$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord(fname))))),
												A2(
												$stil4m$elm_syntax$Combine$ignore,
												$stil4m$elm_syntax$Combine$string('}'),
												A2(
													$stil4m$elm_syntax$Combine$andThen,
													function (ta) {
														return A2(
															$stil4m$elm_syntax$Combine$map,
															$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record,
															additionalRecordFields(
																_List_fromArray(
																	[
																		A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $elm$core$Tuple$pair, fname, ta)
																	])));
													},
													A2(
														$stil4m$elm_syntax$Combine$ignore,
														$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
														A2(
															$stil4m$elm_syntax$Combine$continueWith,
															$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
															A2(
																$stil4m$elm_syntax$Combine$ignore,
																$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
																$stil4m$elm_syntax$Combine$string(':'))))))
											]));
								},
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
									$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName)))
							])),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						$stil4m$elm_syntax$Combine$string('{'))));
		});
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v8) {
			return A2(
				$stil4m$elm_syntax$Combine$andThen,
				function (typeRef) {
					return A2(
						$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayoutWith,
						function (_v9) {
							return $stil4m$elm_syntax$Combine$succeed(typeRef);
						},
						function (_v10) {
							return A2(
								$stil4m$elm_syntax$Combine$or,
								A2(
									$stil4m$elm_syntax$Combine$map,
									function (ta) {
										return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, typeRef, ta);
									},
									A2(
										$stil4m$elm_syntax$Combine$continueWith,
										$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
										A2(
											$stil4m$elm_syntax$Combine$ignore,
											$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
											$stil4m$elm_syntax$Combine$string('->')))),
								$stil4m$elm_syntax$Combine$succeed(typeRef));
						});
				},
				$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFn(0));
		});
}
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$parensTypeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation();
$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation = function () {
	return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$parensTypeAnnotation;
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldDefinition = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition();
$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition = function () {
	return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldDefinition;
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldsTypeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation();
$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation = function () {
	return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldsTypeAnnotation;
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordTypeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation();
$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation = function () {
	return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordTypeAnnotation;
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation();
$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation = function () {
	return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$functionSignatureFromVarPointer = function (varPointer) {
	return A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation,
		A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
			A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Combine$string(':'),
				$stil4m$elm_syntax$Combine$succeed(
					function (ta) {
						return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Signature$Signature, varPointer, ta);
					}))));
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$GLSLExpression = function (a) {
	return {$: 23, a: a};
};
var $elm$parser$Parser$NotNestable = 0;
var $stil4m$elm_syntax$Elm$Parser$Declarations$glslExpression = function () {
	var start = '[glsl|';
	var end = '|]';
	return $stil4m$elm_syntax$Elm$Parser$Node$parser(
		A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Combine$string(end),
			A2(
				$stil4m$elm_syntax$Combine$map,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$String$dropLeft(
						$elm$core$String$length(start)),
					$stil4m$elm_syntax$Elm$Syntax$Expression$GLSLExpression),
				$stil4m$elm_syntax$Combine$fromCore(
					$elm$parser$Parser$getChompedString(
						A3($elm$parser$Parser$multiComment, start, end, 0))))));
}();
var $stil4m$elm_syntax$Elm$Parser$Tokens$ifToken = $stil4m$elm_syntax$Combine$string('if');
var $stil4m$elm_syntax$Elm$Parser$Tokens$allowedOperatorTokens = _List_fromArray(
	['+', '-', ':', '/', '*', '>', '<', '=', '/', '&', '^', '%', '|', '!', '.', '#', '$', '≡', '~', '?', '@']);
var $stil4m$elm_syntax$Elm$Parser$Tokens$excludedOperators = _List_fromArray(
	[':', '->', '--', '=']);
var $stil4m$elm_syntax$Combine$Char$oneOf = function (cs) {
	return A2(
		$stil4m$elm_syntax$Combine$andThen,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Maybe$map($stil4m$elm_syntax$Combine$succeed),
			$elm$core$Maybe$withDefault(
				$stil4m$elm_syntax$Combine$fail(
					'expected one of \'' + ($elm$core$String$fromList(cs) + '\'')))),
		$stil4m$elm_syntax$Combine$Char$satisfy(
			function (a) {
				return A2($elm$core$List$member, a, cs);
			}));
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$operatorTokenFromList = function (allowedChars) {
	return A2(
		$stil4m$elm_syntax$Combine$andThen,
		function (m) {
			return A2($elm$core$List$member, m, $stil4m$elm_syntax$Elm$Parser$Tokens$excludedOperators) ? $stil4m$elm_syntax$Combine$fail('operator is not allowed') : $stil4m$elm_syntax$Combine$succeed(m);
		},
		A2(
			$stil4m$elm_syntax$Combine$map,
			$elm$core$String$fromList,
			$stil4m$elm_syntax$Combine$many1(
				$stil4m$elm_syntax$Combine$Char$oneOf(allowedChars))));
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$infixOperatorToken = $stil4m$elm_syntax$Elm$Parser$Tokens$operatorTokenFromList($stil4m$elm_syntax$Elm$Parser$Tokens$allowedOperatorTokens);
var $stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict = A2(
	$stil4m$elm_syntax$Combine$continueWith,
	$stil4m$elm_syntax$Elm$Parser$Layout$verifyIndent(
		F2(
			function (stateIndent, current) {
				return _Utils_eq(stateIndent, current);
			})),
	$stil4m$elm_syntax$Combine$many1(
		$stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					$stil4m$elm_syntax$Elm$Parser$Layout$anyComment,
					A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$succeed(0),
					$stil4m$elm_syntax$Combine$many1($stil4m$elm_syntax$Elm$Parser$Whitespace$realNewLine)),
					$stil4m$elm_syntax$Elm$Parser$Whitespace$many1Spaces
				]))));
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$liftRecordAccess = function (e) {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v0) {
			return A2(
				$stil4m$elm_syntax$Combine$or,
				A2(
					$stil4m$elm_syntax$Combine$andThen,
					$stil4m$elm_syntax$Elm$Parser$Declarations$liftRecordAccess,
					A2(
						$stil4m$elm_syntax$Combine$map,
						function (f) {
							return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess, e, f);
						},
						A2(
							$stil4m$elm_syntax$Combine$continueWith,
							$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
							$stil4m$elm_syntax$Combine$string('.')))),
				$stil4m$elm_syntax$Combine$succeed(e));
		});
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Literal = function (a) {
	return {$: 11, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$multiLineStringLiteral = function () {
	var helper = function (s) {
		return s.Q ? A2(
			$elm$parser$Parser$map,
			function (v) {
				return $elm$parser$Parser$Loop(
					_Utils_update(
						s,
						{
							Q: false,
							p: A2(
								$elm$core$List$cons,
								$elm$core$String$fromList(
									_List_fromArray(
										[v])),
								s.p)
						}));
			},
			$stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValue) : $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return $elm$parser$Parser$Done(
							$elm$core$String$concat(s.p));
					},
					$elm$parser$Parser$symbol('\"\"\"')),
					A2(
					$elm$parser$Parser$map,
					function (v) {
						return $elm$parser$Parser$Loop(
							_Utils_update(
								s,
								{
									ae: s.ae + 1,
									p: A2($elm$core$List$cons, v, s.p)
								}));
					},
					$elm$parser$Parser$getChompedString(
						$elm$parser$Parser$symbol('\"'))),
					A2(
					$elm$parser$Parser$map,
					function (_v1) {
						return $elm$parser$Parser$Loop(
							_Utils_update(
								s,
								{ae: s.ae + 1, Q: true, p: s.p}));
					},
					$elm$parser$Parser$getChompedString(
						$elm$parser$Parser$symbol('\\'))),
					A2(
					$elm$parser$Parser$andThen,
					function (_v2) {
						var start = _v2.a;
						var value = _v2.b;
						var end = _v2.c;
						return _Utils_eq(start, end) ? $elm$parser$Parser$problem('Expected a string character or a triple double quote') : $elm$parser$Parser$succeed(
							$elm$parser$Parser$Loop(
								_Utils_update(
									s,
									{
										ae: s.ae + 1,
										p: A2($elm$core$List$cons, value, s.p)
									})));
					},
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								$elm$parser$Parser$succeed(
									F3(
										function (start, value, end) {
											return _Utils_Tuple3(start, value, end);
										})),
								$elm$parser$Parser$getOffset),
							$elm$parser$Parser$getChompedString(
								$elm$parser$Parser$chompWhile(
									function (c) {
										return (c !== '\"') && (c !== '\\');
									}))),
						$elm$parser$Parser$getOffset))
				]));
	};
	return $stil4m$elm_syntax$Combine$fromCore(
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$symbol('\"\"\"')),
			A2(
				$elm$parser$Parser$loop,
				{ae: 0, Q: false, p: _List_Nil},
				helper)));
}();
var $stil4m$elm_syntax$Elm$Parser$Declarations$literalExpression = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Elm$Parser$Node$parser(
			A2(
				$stil4m$elm_syntax$Combine$map,
				$stil4m$elm_syntax$Elm$Syntax$Expression$Literal,
				A2($stil4m$elm_syntax$Combine$or, $stil4m$elm_syntax$Elm$Parser$Tokens$multiLineStringLiteral, $stil4m$elm_syntax$Elm$Parser$Tokens$stringLiteral)));
	});
var $stil4m$elm_syntax$Combine$loop = F2(
	function (init, stepper) {
		var wrapper = function (_v3) {
			var oldState = _v3.a;
			var v = _v3.b;
			var _v0 = stepper(v);
			var p = _v0;
			return A2(
				$elm$parser$Parser$map,
				function (_v1) {
					var newState = _v1.a;
					var r = _v1.b;
					if (!r.$) {
						var l = r.a;
						return $elm$parser$Parser$Loop(
							_Utils_Tuple2(newState, l));
					} else {
						var d = r.a;
						return $elm$parser$Parser$Done(
							_Utils_Tuple2(newState, d));
					}
				},
				p(oldState));
		};
		return function (state) {
			return A2(
				$elm$parser$Parser$loop,
				_Utils_Tuple2(state, init),
				wrapper);
		};
	});
var $stil4m$elm_syntax$Elm$Parser$Whitespace$manySpaces = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$chompWhile(
		function (c) {
			return c === ' ';
		}));
var $stil4m$elm_syntax$Elm$Syntax$Expression$Floatable = function (a) {
	return {$: 9, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Hex = function (a) {
	return {$: 8, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Integer = function (a) {
	return {$: 7, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Numbers$forgivingNumber = F3(
	function (floatf, intf, hexf) {
		return $stil4m$elm_syntax$Combine$fromCore(
			$elm$parser$Parser$backtrackable(
				A3($stil4m$elm_syntax$Elm$Parser$Numbers$raw, floatf, intf, hexf)));
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$numberExpression = $stil4m$elm_syntax$Elm$Parser$Node$parser(
	A3($stil4m$elm_syntax$Elm$Parser$Numbers$forgivingNumber, $stil4m$elm_syntax$Elm$Syntax$Expression$Floatable, $stil4m$elm_syntax$Elm$Syntax$Expression$Integer, $stil4m$elm_syntax$Elm$Syntax$Expression$Hex));
var $stil4m$elm_syntax$Elm$Parser$Tokens$ofToken = $stil4m$elm_syntax$Combine$string('of');
var $stil4m$elm_syntax$Elm$Parser$Tokens$allowedPrefixOperatorTokens = A2($elm$core$List$cons, ',', $stil4m$elm_syntax$Elm$Parser$Tokens$allowedOperatorTokens);
var $stil4m$elm_syntax$Elm$Parser$Tokens$prefixOperatorToken = $stil4m$elm_syntax$Elm$Parser$Tokens$operatorTokenFromList($stil4m$elm_syntax$Elm$Parser$Tokens$allowedPrefixOperatorTokens);
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccessFunction = function (a) {
	return {$: 21, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$recordAccessFunctionExpression = $stil4m$elm_syntax$Elm$Parser$Node$parser(
	A2(
		$stil4m$elm_syntax$Combine$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Basics$append('.'),
			$stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccessFunction),
		A2(
			$stil4m$elm_syntax$Combine$continueWith,
			$stil4m$elm_syntax$Elm$Parser$Tokens$functionName,
			$stil4m$elm_syntax$Combine$string('.'))));
var $stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$reference = function () {
	var justFunction = A2(
		$stil4m$elm_syntax$Combine$map,
		function (v) {
			return _Utils_Tuple2(_List_Nil, v);
		},
		$stil4m$elm_syntax$Elm$Parser$Tokens$functionName);
	var helper = function (_v0) {
		var n = _v0.a;
		var xs = _v0.b;
		return $stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								A2(
								$stil4m$elm_syntax$Combine$andThen,
								function (t) {
									return helper(
										_Utils_Tuple2(
											t,
											A2($elm$core$List$cons, n, xs)));
								},
								$stil4m$elm_syntax$Elm$Parser$Tokens$typeName),
								A2(
								$stil4m$elm_syntax$Combine$map,
								function (t) {
									return _Utils_Tuple2(
										t,
										A2($elm$core$List$cons, n, xs));
								},
								$stil4m$elm_syntax$Elm$Parser$Tokens$functionName)
							])),
					$stil4m$elm_syntax$Combine$string('.')),
					$stil4m$elm_syntax$Combine$succeed(
					_Utils_Tuple2(n, xs))
				]));
	};
	var recurring = A2(
		$stil4m$elm_syntax$Combine$map,
		function (_v1) {
			var t = _v1.a;
			var xs = _v1.b;
			return _Utils_Tuple2(
				$elm$core$List$reverse(xs),
				t);
		},
		A2(
			$stil4m$elm_syntax$Combine$andThen,
			function (t) {
				return helper(
					_Utils_Tuple2(t, _List_Nil));
			},
			$stil4m$elm_syntax$Elm$Parser$Tokens$typeName));
	return $stil4m$elm_syntax$Combine$choice(
		_List_fromArray(
			[recurring, justFunction]));
}();
var $stil4m$elm_syntax$Elm$Parser$Declarations$referenceExpression = $stil4m$elm_syntax$Elm$Parser$Node$parser(
	A2(
		$stil4m$elm_syntax$Combine$map,
		function (_v0) {
			var xs = _v0.a;
			var x = _v0.b;
			return A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, xs, x);
		},
		$stil4m$elm_syntax$Elm$Parser$Declarations$reference));
var $stil4m$elm_syntax$Elm$Parser$Tokens$thenToken = $stil4m$elm_syntax$Combine$string('then');
var $stil4m$elm_syntax$Elm$Parser$State$popIndent = function (_v0) {
	var s = _v0;
	return _Utils_update(
		s,
		{
			ap: A2($elm$core$List$drop, 1, s.ap)
		});
};
var $stil4m$elm_syntax$Elm$Parser$State$pushIndent = F2(
	function (x, _v0) {
		var s = _v0;
		return _Utils_update(
			s,
			{
				ap: A2($elm$core$List$cons, x, s.ap)
			});
	});
var $stil4m$elm_syntax$Elm$Parser$State$pushColumn = F2(
	function (col, state) {
		return A2($stil4m$elm_syntax$Elm$Parser$State$pushIndent, col - 1, state);
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$withIndentedState = function (p) {
	return $stil4m$elm_syntax$Combine$withLocation(
		function (location) {
			return A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Combine$modifyState($stil4m$elm_syntax$Elm$Parser$State$popIndent),
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					p,
					$stil4m$elm_syntax$Combine$modifyState(
						$stil4m$elm_syntax$Elm$Parser$State$pushColumn(location.ec))));
		});
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$functionWithNameNode = function (pointer) {
	var functionImplementationFromVarPointer = function (varPointer) {
		return A2(
			$stil4m$elm_syntax$Combine$andMap,
			$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
			A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Combine$string('='),
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						$stil4m$elm_syntax$Combine$many(
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
								$stil4m$elm_syntax$Elm$Parser$Declarations$functionArgument)),
						$stil4m$elm_syntax$Combine$succeed(
							F2(
								function (args, expr) {
									return A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										$stil4m$elm_syntax$Elm$Syntax$Range$combine(
											_List_fromArray(
												[
													$stil4m$elm_syntax$Elm$Syntax$Node$range(varPointer),
													$stil4m$elm_syntax$Elm$Syntax$Node$range(expr)
												])),
										A3($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionImplementation, varPointer, args, expr));
								}))))));
	};
	var functionWithoutSignature = function (varPointer) {
		return A2(
			$stil4m$elm_syntax$Combine$map,
			A2($stil4m$elm_syntax$Elm$Syntax$Expression$Function, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
			functionImplementationFromVarPointer(varPointer));
	};
	var fromParts = F2(
		function (sig, decl) {
			return {
				am: decl,
				ci: $elm$core$Maybe$Nothing,
				fL: $elm$core$Maybe$Just(sig)
			};
		});
	var functionWithSignature = function (varPointer) {
		return A2(
			$stil4m$elm_syntax$Combine$andThen,
			function (sig) {
				return A2(
					$stil4m$elm_syntax$Combine$map,
					fromParts(sig),
					A2(
						$stil4m$elm_syntax$Combine$andThen,
						functionImplementationFromVarPointer,
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							A2(
								$stil4m$elm_syntax$Combine$continueWith,
								$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
								$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict)))));
			},
			$stil4m$elm_syntax$Elm$Parser$Declarations$functionSignatureFromVarPointer(varPointer));
	};
	return $stil4m$elm_syntax$Combine$choice(
		_List_fromArray(
			[
				functionWithSignature(pointer),
				functionWithoutSignature(pointer)
			]));
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$letDestructuringDeclarationWithPattern = function (p) {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v7) {
			return A2(
				$stil4m$elm_syntax$Combine$andMap,
				$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Elm$Parser$Layout$layout,
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$string('='),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							$stil4m$elm_syntax$Combine$succeed(
								$stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring(p))))));
		});
};
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseBlock() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v23) {
			return A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Elm$Parser$Tokens$ofToken,
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
					A2($stil4m$elm_syntax$Combine$continueWith, $stil4m$elm_syntax$Elm$Parser$Layout$layout, $stil4m$elm_syntax$Elm$Parser$Tokens$caseToken)));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseExpression() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v21) {
			return A2(
				$stil4m$elm_syntax$Combine$andThen,
				function (_v22) {
					var start = _v22.a;
					return A2(
						$stil4m$elm_syntax$Combine$map,
						function (cb) {
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								$stil4m$elm_syntax$Elm$Syntax$Range$combine(
									A2(
										$elm$core$List$cons,
										start,
										A2(
											$elm$core$List$map,
											A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $stil4m$elm_syntax$Elm$Syntax$Node$range),
											cb.b8))),
								$stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression(cb));
						},
						A2(
							$stil4m$elm_syntax$Combine$andMap,
							A2(
								$stil4m$elm_syntax$Combine$continueWith,
								$stil4m$elm_syntax$Elm$Parser$Declarations$withIndentedState(
									$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatements()),
								$stil4m$elm_syntax$Elm$Parser$Layout$layout),
							A2(
								$stil4m$elm_syntax$Combine$andMap,
								$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseBlock(),
								$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Expression$CaseBlock))));
				},
				$stil4m$elm_syntax$Elm$Parser$Node$parser(
					$stil4m$elm_syntax$Combine$succeed(0)));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatement() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v20) {
			return A2(
				$stil4m$elm_syntax$Combine$andMap,
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						A2(
							$stil4m$elm_syntax$Combine$continueWith,
							$stil4m$elm_syntax$Combine$string('->'),
							$stil4m$elm_syntax$Combine$maybe(
								A2($stil4m$elm_syntax$Combine$or, $stil4m$elm_syntax$Elm$Parser$Layout$layout, $stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict))))),
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					$stil4m$elm_syntax$Elm$Parser$Patterns$pattern,
					$stil4m$elm_syntax$Combine$succeed($elm$core$Tuple$pair)));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatements() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v19) {
			var helper = function (last) {
				return $stil4m$elm_syntax$Combine$withState(
					function (s) {
						return $stil4m$elm_syntax$Combine$withLocation(
							function (l) {
								return _Utils_eq(
									$stil4m$elm_syntax$Elm$Parser$State$expectedColumn(s),
									l.ec) ? A2(
									$stil4m$elm_syntax$Combine$map,
									function (c) {
										return $stil4m$elm_syntax$Combine$Loop(
											A2($elm$core$List$cons, c, last));
									},
									$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatement()) : $stil4m$elm_syntax$Combine$succeed(
									$stil4m$elm_syntax$Combine$Done(
										$elm$core$List$reverse(last)));
							});
					});
			};
			return A2(
				$stil4m$elm_syntax$Combine$andThen,
				function (v) {
					return A2($stil4m$elm_syntax$Combine$loop, v, helper);
				},
				A2(
					$stil4m$elm_syntax$Combine$map,
					$elm$core$List$singleton,
					$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatement()));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v15) {
			return A2(
				$stil4m$elm_syntax$Combine$andThen,
				function (first) {
					var complete = function (rest) {
						return $stil4m$elm_syntax$Combine$succeed(
							function () {
								if (!rest.b) {
									return first;
								} else {
									return A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										$stil4m$elm_syntax$Elm$Syntax$Range$combine(
											A2(
												$elm$core$List$cons,
												$stil4m$elm_syntax$Elm$Syntax$Node$range(first),
												A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$range, rest))),
										$stil4m$elm_syntax$Elm$Syntax$Expression$Application(
											A2(
												$elm$core$List$cons,
												first,
												$elm$core$List$reverse(rest))));
								}
							}());
					};
					var promoter = function (rest) {
						return A2(
							$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayoutWith,
							function (_v16) {
								return complete(rest);
							},
							function (_v17) {
								return A2(
									$stil4m$elm_syntax$Combine$or,
									A2(
										$stil4m$elm_syntax$Combine$andThen,
										function (next) {
											return promoter(
												A2($elm$core$List$cons, next, rest));
										},
										$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expressionNotApplication()),
									complete(rest));
							});
					};
					return promoter(_List_Nil);
				},
				$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expressionNotApplication());
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expressionNotApplication() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v14) {
			return A2(
				$stil4m$elm_syntax$Combine$andThen,
				$stil4m$elm_syntax$Elm$Parser$Declarations$liftRecordAccess,
				$stil4m$elm_syntax$Combine$choice(
					_List_fromArray(
						[
							$stil4m$elm_syntax$Elm$Parser$Declarations$numberExpression,
							$stil4m$elm_syntax$Elm$Parser$Declarations$referenceExpression,
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$ifBlockExpression(),
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$tupledExpression(),
							$stil4m$elm_syntax$Elm$Parser$Declarations$recordAccessFunctionExpression,
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$operatorExpression(),
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letExpression(),
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$lambdaExpression(),
							$stil4m$elm_syntax$Elm$Parser$Declarations$literalExpression,
							$stil4m$elm_syntax$Elm$Parser$Declarations$charLiteralExpression,
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$recordExpression(),
							$stil4m$elm_syntax$Elm$Parser$Declarations$glslExpression,
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$listExpression(),
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseExpression()
						])));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$ifBlockExpression() {
	return $stil4m$elm_syntax$Elm$Parser$Node$parser(
		A2(
			$stil4m$elm_syntax$Combine$continueWith,
			$stil4m$elm_syntax$Combine$lazy(
				function (_v13) {
					return A2(
						$stil4m$elm_syntax$Combine$andMap,
						A2(
							$stil4m$elm_syntax$Combine$continueWith,
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
							A2($stil4m$elm_syntax$Combine$continueWith, $stil4m$elm_syntax$Elm$Parser$Layout$layout, $stil4m$elm_syntax$Elm$Parser$Tokens$elseToken)),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							A2(
								$stil4m$elm_syntax$Combine$andMap,
								$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
									A2(
										$stil4m$elm_syntax$Combine$ignore,
										$stil4m$elm_syntax$Elm$Parser$Tokens$thenToken,
										A2(
											$stil4m$elm_syntax$Combine$ignore,
											$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
											A2(
												$stil4m$elm_syntax$Combine$andMap,
												$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
												A2(
													$stil4m$elm_syntax$Combine$ignore,
													$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
													$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock)))))))));
				}),
			$stil4m$elm_syntax$Elm$Parser$Tokens$ifToken));
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$lambdaExpression() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v12) {
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
						$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
							$stil4m$elm_syntax$Combine$string('->'))),
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						A2(
							$stil4m$elm_syntax$Combine$sepBy1,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							$stil4m$elm_syntax$Elm$Parser$Declarations$functionArgument),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$string('\\'),
								$stil4m$elm_syntax$Combine$succeed(
									F2(
										function (args, expr) {
											return $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression(
												A2($stil4m$elm_syntax$Elm$Syntax$Expression$Lambda, args, expr));
										})))))));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBlock() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v11) {
			return A2(
				$stil4m$elm_syntax$Combine$ignore,
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$string('in'),
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[$stil4m$elm_syntax$Elm$Parser$Layout$layout, $stil4m$elm_syntax$Elm$Parser$Whitespace$manySpaces]))),
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Elm$Parser$Declarations$withIndentedState(
						$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBody()),
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Layout$layout,
						$stil4m$elm_syntax$Combine$string('let'))));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBody() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v8) {
			var blockElement = A2(
				$stil4m$elm_syntax$Combine$andThen,
				function (_v9) {
					var r = _v9.a;
					var p = _v9.b;
					if (p.$ === 11) {
						var v = p.a;
						return A2(
							$stil4m$elm_syntax$Combine$map,
							$stil4m$elm_syntax$Elm$Syntax$Expression$LetFunction,
							$stil4m$elm_syntax$Elm$Parser$Declarations$functionWithNameNode(
								A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, v)));
					} else {
						return $stil4m$elm_syntax$Elm$Parser$Declarations$letDestructuringDeclarationWithPattern(
							A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, p));
					}
				},
				$stil4m$elm_syntax$Elm$Parser$Patterns$pattern);
			return A2(
				$stil4m$elm_syntax$Combine$andMap,
				$stil4m$elm_syntax$Combine$many(
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						$stil4m$elm_syntax$Elm$Parser$Node$parser(blockElement))),
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					$stil4m$elm_syntax$Elm$Parser$Node$parser(blockElement),
					$stil4m$elm_syntax$Combine$succeed($elm$core$List$cons)));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letExpression() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v6) {
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
						$stil4m$elm_syntax$Elm$Parser$Layout$layout),
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBlock(),
						$stil4m$elm_syntax$Combine$succeed(
							function (decls) {
								return A2(
									$elm$core$Basics$composeR,
									$stil4m$elm_syntax$Elm$Syntax$Expression$LetBlock(decls),
									$stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression);
							}))));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$listExpression() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v5) {
			var innerExpressions = A2(
				$stil4m$elm_syntax$Combine$map,
				$stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr,
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					$stil4m$elm_syntax$Combine$many(
						A2(
							$stil4m$elm_syntax$Combine$continueWith,
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
								$stil4m$elm_syntax$Combine$string(',')))),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						A2(
							$stil4m$elm_syntax$Combine$andMap,
							$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
							$stil4m$elm_syntax$Combine$succeed($elm$core$List$cons)))));
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								A2(
								$stil4m$elm_syntax$Combine$map,
								$elm$core$Basics$always(
									$stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr(_List_Nil)),
								$stil4m$elm_syntax$Combine$string(']')),
								A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$string(']'),
								innerExpressions)
							])),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						$stil4m$elm_syntax$Combine$string('['))));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$operatorExpression() {
	var negationExpression = $stil4m$elm_syntax$Combine$lazy(
		function (_v4) {
			return A2(
				$stil4m$elm_syntax$Combine$map,
				$stil4m$elm_syntax$Elm$Syntax$Expression$Negation,
				A2(
					$stil4m$elm_syntax$Combine$andThen,
					$stil4m$elm_syntax$Elm$Parser$Declarations$liftRecordAccess,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								$stil4m$elm_syntax$Elm$Parser$Declarations$referenceExpression,
								$stil4m$elm_syntax$Elm$Parser$Declarations$numberExpression,
								$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$tupledExpression()
							]))));
		});
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v3) {
			return $stil4m$elm_syntax$Combine$choice(
				_List_fromArray(
					[
						$stil4m$elm_syntax$Elm$Parser$Node$parser(
						A2(
							$stil4m$elm_syntax$Combine$continueWith,
							$stil4m$elm_syntax$Combine$choice(
								_List_fromArray(
									[
										negationExpression,
										A2(
										$stil4m$elm_syntax$Combine$ignore,
										$stil4m$elm_syntax$Elm$Parser$Layout$layout,
										$stil4m$elm_syntax$Combine$succeed(
											$stil4m$elm_syntax$Elm$Syntax$Expression$Operator('-')))
									])),
							$stil4m$elm_syntax$Combine$string('-'))),
						$stil4m$elm_syntax$Elm$Parser$Node$parser(
						A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Expression$Operator, $stil4m$elm_syntax$Elm$Parser$Tokens$infixOperatorToken))
					]));
		});
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$recordExpression() {
	return $stil4m$elm_syntax$Elm$Parser$Node$parser(
		$stil4m$elm_syntax$Combine$lazy(
			function (_v2) {
				var recordField = $stil4m$elm_syntax$Elm$Parser$Node$parser(
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$string('='),
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
									A2(
										$stil4m$elm_syntax$Combine$andMap,
										$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
										$stil4m$elm_syntax$Combine$succeed($elm$core$Tuple$pair)))))));
				var recordFields = A2(
					$stil4m$elm_syntax$Combine$andMap,
					$stil4m$elm_syntax$Combine$many(
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							A2(
								$stil4m$elm_syntax$Combine$continueWith,
								recordField,
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
									$stil4m$elm_syntax$Combine$string(','))))),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						A2(
							$stil4m$elm_syntax$Combine$andMap,
							recordField,
							$stil4m$elm_syntax$Combine$succeed($elm$core$List$cons))));
				var recordUpdateSyntaxParser = function (fname) {
					return A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$string('}'),
						A2(
							$stil4m$elm_syntax$Combine$map,
							function (e) {
								return A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression, fname, e);
							},
							A2(
								$stil4m$elm_syntax$Combine$continueWith,
								recordFields,
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
									$stil4m$elm_syntax$Combine$string('|')))));
				};
				var recordContents = A2(
					$stil4m$elm_syntax$Combine$andThen,
					function (fname) {
						return $stil4m$elm_syntax$Combine$choice(
							_List_fromArray(
								[
									recordUpdateSyntaxParser(fname),
									A2(
									$stil4m$elm_syntax$Combine$andThen,
									function (fieldUpdate) {
										return $stil4m$elm_syntax$Combine$choice(
											_List_fromArray(
												[
													A2(
													$stil4m$elm_syntax$Combine$map,
													$elm$core$Basics$always(
														$stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr(
															_List_fromArray(
																[fieldUpdate]))),
													$stil4m$elm_syntax$Combine$string('}')),
													A2(
													$stil4m$elm_syntax$Combine$ignore,
													$stil4m$elm_syntax$Combine$string('}'),
													A2(
														$stil4m$elm_syntax$Combine$map,
														function (fieldUpdates) {
															return $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr(
																A2($elm$core$List$cons, fieldUpdate, fieldUpdates));
														},
														A2(
															$stil4m$elm_syntax$Combine$continueWith,
															recordFields,
															A2(
																$stil4m$elm_syntax$Combine$ignore,
																$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
																$stil4m$elm_syntax$Combine$string(',')))))
												]));
									},
									A2(
										$stil4m$elm_syntax$Combine$ignore,
										$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
										A2(
											$stil4m$elm_syntax$Combine$continueWith,
											A2(
												$stil4m$elm_syntax$Combine$map,
												function (e) {
													return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $elm$core$Tuple$pair, fname, e);
												},
												$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression()),
											A2(
												$stil4m$elm_syntax$Combine$ignore,
												$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
												$stil4m$elm_syntax$Combine$string('=')))))
								]));
					},
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName)));
				return A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								A2(
								$stil4m$elm_syntax$Combine$map,
								$elm$core$Basics$always(
									$stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr(_List_Nil)),
								$stil4m$elm_syntax$Combine$string('}')),
								recordContents
							])),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
						$stil4m$elm_syntax$Combine$string('{')));
			}));
}
function $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$tupledExpression() {
	return $stil4m$elm_syntax$Combine$lazy(
		function (_v0) {
			var commaSep = $stil4m$elm_syntax$Combine$many(
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
					A2(
						$stil4m$elm_syntax$Combine$continueWith,
						$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							$stil4m$elm_syntax$Combine$string(',')))));
			var closingParen = $stil4m$elm_syntax$Combine$fromCore(
				$elm$parser$Parser$symbol(')'));
			var asExpression = F2(
				function (x, xs) {
					if (!xs.b) {
						return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(x);
					} else {
						return $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
							A2($elm$core$List$cons, x, xs));
					}
				});
			var nested = A2(
				$stil4m$elm_syntax$Combine$andMap,
				commaSep,
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression(),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
							$stil4m$elm_syntax$Combine$succeed(asExpression)))));
			return $stil4m$elm_syntax$Elm$Parser$Node$parser(
				A2(
					$stil4m$elm_syntax$Combine$continueWith,
					$stil4m$elm_syntax$Combine$choice(
						_List_fromArray(
							[
								A2(
								$stil4m$elm_syntax$Combine$map,
								$elm$core$Basics$always($stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr),
								closingParen),
								$stil4m$elm_syntax$Combine$backtrackable(
								A2(
									$stil4m$elm_syntax$Combine$map,
									$stil4m$elm_syntax$Elm$Syntax$Expression$PrefixOperator,
									A2($stil4m$elm_syntax$Combine$ignore, closingParen, $stil4m$elm_syntax$Elm$Parser$Tokens$prefixOperatorToken))),
								A2($stil4m$elm_syntax$Combine$ignore, closingParen, nested)
							])),
					$stil4m$elm_syntax$Combine$fromCore(
						$elm$parser$Parser$symbol('('))));
		});
}
var $stil4m$elm_syntax$Elm$Parser$Declarations$caseBlock = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseBlock();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseBlock = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$caseBlock;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$caseExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$caseExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$caseStatement = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatement();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatement = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$caseStatement;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$caseStatements = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatements();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$caseStatements = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$caseStatements;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$expression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$expression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$expressionNotApplication = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expressionNotApplication();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$expressionNotApplication = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$expressionNotApplication;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$ifBlockExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$ifBlockExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$ifBlockExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$ifBlockExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$lambdaExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$lambdaExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$lambdaExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$lambdaExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$letBlock = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBlock();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBlock = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$letBlock;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$letBody = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBody();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letBody = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$letBody;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$letExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$letExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$letExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$listExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$listExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$listExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$listExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$operatorExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$operatorExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$operatorExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$operatorExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$recordExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$recordExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$recordExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$recordExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$tupledExpression = $stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$tupledExpression();
$stil4m$elm_syntax$Elm$Parser$Declarations$cyclic$tupledExpression = function () {
	return $stil4m$elm_syntax$Elm$Parser$Declarations$tupledExpression;
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$destructuringDeclaration = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return A2(
			$stil4m$elm_syntax$Combine$andMap,
			$stil4m$elm_syntax$Elm$Parser$Declarations$expression,
			A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Elm$Parser$Layout$layout,
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Combine$string('='),
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						$stil4m$elm_syntax$Elm$Parser$Patterns$pattern,
						$stil4m$elm_syntax$Combine$succeed(
							F2(
								function (x, y) {
									return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Declaration$Destructuring, x, y);
								}))))));
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$functionRange = function (_function) {
	return $stil4m$elm_syntax$Elm$Syntax$Range$combine(
		_List_fromArray(
			[
				function () {
				var _v0 = _function.ci;
				if (!_v0.$) {
					var documentation = _v0.a;
					return $stil4m$elm_syntax$Elm$Syntax$Node$range(documentation);
				} else {
					return A2(
						$elm$core$Maybe$withDefault,
						function (_v3) {
							var r = _v3.a;
							return r;
						}(
							$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am).cN),
						A2(
							$elm$core$Maybe$map,
							function (_v1) {
								var value = _v1.b;
								var _v2 = value.cN;
								var r = _v2.a;
								return r;
							},
							_function.fL));
				}
			}(),
				function (_v4) {
				var r = _v4.a;
				return r;
			}(
				$stil4m$elm_syntax$Elm$Syntax$Node$value(_function.am)._)
			]));
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$function = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return A2(
			$stil4m$elm_syntax$Combine$map,
			function (f) {
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					$stil4m$elm_syntax$Elm$Syntax$Expression$functionRange(f),
					$stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(f));
			},
			A2(
				$stil4m$elm_syntax$Combine$andThen,
				$stil4m$elm_syntax$Elm$Parser$Declarations$functionWithNameNode,
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
					$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName))));
	});
var $stil4m$elm_syntax$Elm$Syntax$Declaration$InfixDeclaration = function (a) {
	return {$: 4, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Infix$Infix = F4(
	function (direction, precedence, operator, _function) {
		return {m: direction, n: _function, o: operator, q: precedence};
	});
var $stil4m$elm_syntax$Elm$Parser$Infix$infixDirection = $stil4m$elm_syntax$Combine$choice(
	_List_fromArray(
		[
			A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Combine$string('right'),
			$stil4m$elm_syntax$Combine$succeed(1)),
			A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Combine$string('left'),
			$stil4m$elm_syntax$Combine$succeed(0)),
			A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Combine$string('non'),
			$stil4m$elm_syntax$Combine$succeed(2))
		]));
var $elm$parser$Parser$Advanced$int = F2(
	function (expecting, invalid) {
		return $elm$parser$Parser$Advanced$number(
			{
				b6: $elm$core$Result$Err(invalid),
				cl: expecting,
				cn: $elm$core$Result$Err(invalid),
				cz: $elm$core$Result$Err(invalid),
				cE: $elm$core$Result$Ok($elm$core$Basics$identity),
				eW: invalid,
				cQ: $elm$core$Result$Err(invalid)
			});
	});
var $elm$parser$Parser$int = A2($elm$parser$Parser$Advanced$int, $elm$parser$Parser$ExpectingInt, $elm$parser$Parser$ExpectingInt);
var $stil4m$elm_syntax$Combine$Num$int = $stil4m$elm_syntax$Combine$fromCore($elm$parser$Parser$int);
var $stil4m$elm_syntax$Elm$Parser$Infix$infixDefinition = A2(
	$stil4m$elm_syntax$Combine$andMap,
	$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
	A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Elm$Parser$Layout$layout,
		A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Combine$string('='),
			A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Elm$Parser$Layout$layout,
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					$stil4m$elm_syntax$Elm$Parser$Node$parser(
						$stil4m$elm_syntax$Combine$parens($stil4m$elm_syntax$Elm$Parser$Tokens$prefixOperatorToken)),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Elm$Parser$Layout$layout,
						A2(
							$stil4m$elm_syntax$Combine$andMap,
							$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Combine$Num$int),
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Elm$Parser$Layout$layout,
								A2(
									$stil4m$elm_syntax$Combine$andMap,
									$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Infix$infixDirection),
									A2(
										$stil4m$elm_syntax$Combine$ignore,
										$stil4m$elm_syntax$Elm$Parser$Layout$layout,
										A2(
											$stil4m$elm_syntax$Combine$ignore,
											$stil4m$elm_syntax$Combine$fromCore(
												$elm$parser$Parser$keyword('infix')),
											$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Infix$Infix))))))))))));
var $stil4m$elm_syntax$Elm$Parser$Ranges$asPointerLocation = function (_v0) {
	var line = _v0.aJ;
	var column = _v0.ec;
	return {ec: column, fD: line};
};
var $stil4m$elm_syntax$Elm$Parser$Ranges$withCurrentPoint = function (p) {
	return $stil4m$elm_syntax$Combine$withLocation(
		function (start) {
			var k = $stil4m$elm_syntax$Elm$Parser$Ranges$asPointerLocation(start);
			return p(
				{eq: k, fT: k});
		});
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$infixDeclaration = $stil4m$elm_syntax$Elm$Parser$Ranges$withCurrentPoint(
	function (current) {
		return A2(
			$stil4m$elm_syntax$Combine$map,
			function (inf) {
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					$stil4m$elm_syntax$Elm$Syntax$Range$combine(
						_List_fromArray(
							[
								current,
								$stil4m$elm_syntax$Elm$Syntax$Node$range(inf.n)
							])),
					$stil4m$elm_syntax$Elm$Syntax$Declaration$InfixDeclaration(inf));
			},
			$stil4m$elm_syntax$Elm$Parser$Infix$infixDefinition);
	});
var $stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$portToken = $stil4m$elm_syntax$Combine$string('port');
var $stil4m$elm_syntax$Elm$Parser$Declarations$signature = A2(
	$stil4m$elm_syntax$Combine$andMap,
	A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation,
		A2(
			$stil4m$elm_syntax$Combine$continueWith,
			$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
				$stil4m$elm_syntax$Combine$string(':')))),
	A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName),
		$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Signature$Signature)));
var $stil4m$elm_syntax$Elm$Parser$Declarations$portDeclaration = $stil4m$elm_syntax$Elm$Parser$Ranges$withCurrentPoint(
	function (current) {
		return A2(
			$stil4m$elm_syntax$Combine$map,
			function (sig) {
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					$stil4m$elm_syntax$Elm$Syntax$Range$combine(
						_List_fromArray(
							[
								current,
								function (_v0) {
								var r = _v0.a;
								return r;
							}(sig.dn)
							])),
					$stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration(sig));
			},
			A2(
				$stil4m$elm_syntax$Combine$continueWith,
				$stil4m$elm_syntax$Elm$Parser$Declarations$signature,
				A2($stil4m$elm_syntax$Combine$ignore, $stil4m$elm_syntax$Elm$Parser$Layout$layout, $stil4m$elm_syntax$Elm$Parser$Tokens$portToken)));
	});
var $stil4m$elm_syntax$Elm$Parser$Typings$DefinedAlias = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Parser$Typings$DefinedType = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Type$Type = F4(
	function (documentation, name, generics, constructors) {
		return {ed: constructors, ci: documentation, cr: generics, cN: name};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAlias$TypeAlias = F4(
	function (documentation, name, generics, typeAnnotation) {
		return {ci: documentation, cr: generics, cN: name, dn: typeAnnotation};
	});
var $stil4m$elm_syntax$Elm$Parser$Typings$genericList = $stil4m$elm_syntax$Combine$many(
	A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Elm$Parser$Layout$layout,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$functionName)));
var $stil4m$elm_syntax$Elm$Parser$Typings$typePrefix = A2(
	$stil4m$elm_syntax$Combine$continueWith,
	$stil4m$elm_syntax$Elm$Parser$Layout$layout,
	$stil4m$elm_syntax$Combine$string('type'));
var $stil4m$elm_syntax$Elm$Syntax$Type$ValueConstructor = F2(
	function (name, _arguments) {
		return {b4: _arguments, cN: name};
	});
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNonGreedy = $stil4m$elm_syntax$Combine$choice(
	_List_fromArray(
		[
			$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$parensTypeAnnotation,
			$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typedTypeAnnotation(1),
			$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$genericTypeAnnotation,
			$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordTypeAnnotation
		]));
var $stil4m$elm_syntax$Elm$Parser$Typings$valueConstructor = A2(
	$stil4m$elm_syntax$Combine$andThen,
	function (tnn) {
		var range = tnn.a;
		var complete = function (args) {
			return $stil4m$elm_syntax$Combine$succeed(
				A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					$stil4m$elm_syntax$Elm$Syntax$Range$combine(
						A2(
							$elm$core$List$cons,
							range,
							A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$range, args))),
					A2($stil4m$elm_syntax$Elm$Syntax$Type$ValueConstructor, tnn, args)));
		};
		var argHelper = function (xs) {
			return A2(
				$stil4m$elm_syntax$Combine$continueWith,
				$stil4m$elm_syntax$Combine$choice(
					_List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Combine$andThen,
							function (ta) {
								return A2(
									$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayoutWith,
									function (_v0) {
										return $stil4m$elm_syntax$Combine$succeed(
											$elm$core$List$reverse(
												A2($elm$core$List$cons, ta, xs)));
									},
									function (_v1) {
										return argHelper(
											A2($elm$core$List$cons, ta, xs));
									});
							},
							$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNonGreedy),
							$stil4m$elm_syntax$Combine$succeed(
							$elm$core$List$reverse(xs))
						])),
				$stil4m$elm_syntax$Combine$succeed(0));
		};
		return A2(
			$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayoutWith,
			function (_v2) {
				return complete(_List_Nil);
			},
			function (_v3) {
				return A2(
					$stil4m$elm_syntax$Combine$andThen,
					complete,
					argHelper(_List_Nil));
			});
	},
	A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$typeName),
		$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Type$ValueConstructor)));
var $stil4m$elm_syntax$Elm$Parser$Typings$valueConstructors = A2(
	$stil4m$elm_syntax$Combine$sepBy1,
	A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
		$stil4m$elm_syntax$Combine$string('|')),
	$stil4m$elm_syntax$Elm$Parser$Typings$valueConstructor);
var $stil4m$elm_syntax$Elm$Parser$Typings$typeDefinition = $stil4m$elm_syntax$Elm$Parser$Ranges$withCurrentPoint(
	function (start) {
		return A2(
			$stil4m$elm_syntax$Combine$continueWith,
			$stil4m$elm_syntax$Combine$choice(
				_List_fromArray(
					[
						A2(
						$stil4m$elm_syntax$Combine$map,
						function (typeAlias) {
							return A2(
								$stil4m$elm_syntax$Elm$Parser$Typings$DefinedAlias,
								$stil4m$elm_syntax$Elm$Syntax$Range$combine(
									_List_fromArray(
										[
											start,
											$stil4m$elm_syntax$Elm$Syntax$Node$range(typeAlias.dn)
										])),
								typeAlias);
						},
						A2(
							$stil4m$elm_syntax$Combine$andMap,
							$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation,
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Elm$Parser$Layout$layout,
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$string('='),
									A2(
										$stil4m$elm_syntax$Combine$andMap,
										$stil4m$elm_syntax$Elm$Parser$Typings$genericList,
										A2(
											$stil4m$elm_syntax$Combine$andMap,
											A2(
												$stil4m$elm_syntax$Combine$ignore,
												$stil4m$elm_syntax$Elm$Parser$Layout$layout,
												$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$typeName)),
											A2(
												$stil4m$elm_syntax$Combine$ignore,
												A2(
													$stil4m$elm_syntax$Combine$continueWith,
													$stil4m$elm_syntax$Elm$Parser$Layout$layout,
													$stil4m$elm_syntax$Combine$string('alias')),
												$stil4m$elm_syntax$Combine$succeed(
													$stil4m$elm_syntax$Elm$Syntax$TypeAlias$TypeAlias($elm$core$Maybe$Nothing))))))))),
						A2(
						$stil4m$elm_syntax$Combine$map,
						function (tipe) {
							return A2(
								$stil4m$elm_syntax$Elm$Parser$Typings$DefinedType,
								$stil4m$elm_syntax$Elm$Syntax$Range$combine(
									A2(
										$elm$core$List$cons,
										start,
										A2(
											$elm$core$List$map,
											function (_v0) {
												var r = _v0.a;
												return r;
											},
											tipe.ed))),
								tipe);
						},
						A2(
							$stil4m$elm_syntax$Combine$andMap,
							$stil4m$elm_syntax$Elm$Parser$Typings$valueConstructors,
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
									$stil4m$elm_syntax$Combine$string('=')),
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
									A2(
										$stil4m$elm_syntax$Combine$andMap,
										$stil4m$elm_syntax$Elm$Parser$Typings$genericList,
										A2(
											$stil4m$elm_syntax$Combine$ignore,
											$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
											A2(
												$stil4m$elm_syntax$Combine$andMap,
												$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$typeName),
												$stil4m$elm_syntax$Combine$succeed(
													$stil4m$elm_syntax$Elm$Syntax$Type$Type($elm$core$Maybe$Nothing)))))))))
					])),
			$stil4m$elm_syntax$Elm$Parser$Typings$typePrefix);
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$declaration = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					$stil4m$elm_syntax$Elm$Parser$Declarations$infixDeclaration,
					$stil4m$elm_syntax$Elm$Parser$Declarations$function,
					A2(
					$stil4m$elm_syntax$Combine$map,
					function (v) {
						if (!v.$) {
							var r = v.a;
							var t = v.b;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								r,
								$stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration(t));
						} else {
							var r = v.a;
							var a = v.b;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								r,
								$stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration(a));
						}
					},
					$stil4m$elm_syntax$Elm$Parser$Typings$typeDefinition),
					$stil4m$elm_syntax$Elm$Parser$Declarations$portDeclaration,
					$stil4m$elm_syntax$Elm$Parser$Declarations$destructuringDeclaration
				]));
	});
var $stil4m$elm_syntax$Elm$Parser$File$fileDeclarations = $stil4m$elm_syntax$Combine$many(
	A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict),
		$stil4m$elm_syntax$Elm$Parser$Declarations$declaration));
var $stil4m$elm_syntax$Elm$Parser$Tokens$asToken = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$keyword('as'));
var $stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Expose$functionExpose = $stil4m$elm_syntax$Elm$Parser$Node$parser(
	A2($stil4m$elm_syntax$Combine$map, $stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose, $stil4m$elm_syntax$Elm$Parser$Tokens$functionName));
var $stil4m$elm_syntax$Combine$while = function (pred) {
	return function (state) {
		return A2(
			$elm$parser$Parser$map,
			function (x) {
				return _Utils_Tuple2(state, x);
			},
			$elm$parser$Parser$getChompedString(
				$elm$parser$Parser$chompWhile(pred)));
	};
};
var $stil4m$elm_syntax$Elm$Parser$Expose$infixExpose = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Elm$Parser$Node$parser(
			A2(
				$stil4m$elm_syntax$Combine$map,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose,
				$stil4m$elm_syntax$Combine$parens(
					$stil4m$elm_syntax$Combine$while(
						$elm$core$Basics$neq(')')))));
	});
var $stil4m$elm_syntax$Elm$Parser$Expose$exposedType = A2(
	$stil4m$elm_syntax$Combine$andThen,
	function (tipe) {
		return $stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Combine$map,
					A2(
						$elm$core$Basics$composeR,
						$stil4m$elm_syntax$Elm$Syntax$Node$range,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Maybe$Just,
							A2(
								$elm$core$Basics$composeR,
								function (v) {
									return A2($stil4m$elm_syntax$Elm$Syntax$Exposing$ExposedType, tipe, v);
								},
								$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose))),
					$stil4m$elm_syntax$Elm$Parser$Node$parser(
						$stil4m$elm_syntax$Combine$parens(
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
								$stil4m$elm_syntax$Combine$string('..'))))),
					$stil4m$elm_syntax$Combine$succeed(
					$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose(tipe))
				]));
	},
	A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
		A2(
			$stil4m$elm_syntax$Combine$andMap,
			$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
			$stil4m$elm_syntax$Combine$succeed($elm$core$Basics$identity))));
var $stil4m$elm_syntax$Elm$Parser$Expose$typeExpose = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Expose$exposedType);
	});
var $stil4m$elm_syntax$Elm$Parser$Expose$exposable = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return $stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[$stil4m$elm_syntax$Elm$Parser$Expose$typeExpose, $stil4m$elm_syntax$Elm$Parser$Expose$infixExpose, $stil4m$elm_syntax$Elm$Parser$Expose$functionExpose]));
	});
var $stil4m$elm_syntax$Elm$Parser$Ranges$withRange = function (p) {
	return $stil4m$elm_syntax$Combine$withLocation(
		function (start) {
			return A2(
				$stil4m$elm_syntax$Combine$andMap,
				$stil4m$elm_syntax$Combine$withLocation(
					function (end) {
						return $stil4m$elm_syntax$Combine$succeed(
							{
								eq: $stil4m$elm_syntax$Elm$Parser$Ranges$asPointerLocation(end),
								fT: $stil4m$elm_syntax$Elm$Parser$Ranges$asPointerLocation(start)
							});
					}),
				p);
		});
};
var $stil4m$elm_syntax$Elm$Parser$Expose$exposingListInner = $stil4m$elm_syntax$Combine$lazy(
	function (_v0) {
		return A2(
			$stil4m$elm_syntax$Combine$or,
			$stil4m$elm_syntax$Elm$Parser$Ranges$withRange(
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
						$stil4m$elm_syntax$Combine$string('..')),
					$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Exposing$All))),
			A2(
				$stil4m$elm_syntax$Combine$map,
				$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit,
				A2(
					$stil4m$elm_syntax$Combine$sepBy,
					$stil4m$elm_syntax$Combine$Char$char(','),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides($stil4m$elm_syntax$Elm$Parser$Expose$exposable))));
	});
var $stil4m$elm_syntax$Elm$Parser$Expose$exposeListWith = $stil4m$elm_syntax$Combine$parens(
	A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		A2($stil4m$elm_syntax$Combine$continueWith, $stil4m$elm_syntax$Elm$Parser$Expose$exposingListInner, $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout)));
var $stil4m$elm_syntax$Elm$Parser$Tokens$exposingToken = $stil4m$elm_syntax$Combine$string('exposing');
var $stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition = A2(
	$stil4m$elm_syntax$Combine$continueWith,
	$stil4m$elm_syntax$Elm$Parser$Expose$exposeListWith,
	A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layout),
		$stil4m$elm_syntax$Elm$Parser$Tokens$exposingToken));
var $stil4m$elm_syntax$Elm$Parser$Tokens$importToken = $stil4m$elm_syntax$Combine$fromCore(
	$elm$parser$Parser$keyword('import'));
var $stil4m$elm_syntax$Elm$Parser$Base$moduleName = A2(
	$stil4m$elm_syntax$Combine$sepBy1,
	$stil4m$elm_syntax$Combine$string('.'),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeName);
var $stil4m$elm_syntax$Elm$Parser$Imports$setupNode = F2(
	function (start, imp) {
		var allRanges = _List_fromArray(
			[
				$elm$core$Maybe$Just(start),
				$elm$core$Maybe$Just(
				$stil4m$elm_syntax$Elm$Syntax$Node$range(imp.bs)),
				A2($elm$core$Maybe$map, $stil4m$elm_syntax$Elm$Syntax$Node$range, imp.ew),
				A2($elm$core$Maybe$map, $stil4m$elm_syntax$Elm$Syntax$Node$range, imp.e5)
			]);
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			$stil4m$elm_syntax$Elm$Syntax$Range$combine(
				A2($elm$core$List$filterMap, $elm$core$Basics$identity, allRanges)),
			imp);
	});
var $stil4m$elm_syntax$Elm$Parser$Imports$importDefinition = function () {
	var parseExposingDefinition = F2(
		function (mod, asDef) {
			return $stil4m$elm_syntax$Combine$choice(
				_List_fromArray(
					[
						A2(
						$stil4m$elm_syntax$Combine$map,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Maybe$Just,
							A2($stil4m$elm_syntax$Elm$Syntax$Import$Import, mod, asDef)),
						$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition)),
						$stil4m$elm_syntax$Combine$succeed(
						A3($stil4m$elm_syntax$Elm$Syntax$Import$Import, mod, asDef, $elm$core$Maybe$Nothing))
					]));
		});
	var importAndModuleName = A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Base$moduleName),
		A2($stil4m$elm_syntax$Combine$continueWith, $stil4m$elm_syntax$Elm$Parser$Layout$layout, $stil4m$elm_syntax$Elm$Parser$Tokens$importToken));
	var asDefinition = A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Base$moduleName),
		A2($stil4m$elm_syntax$Combine$continueWith, $stil4m$elm_syntax$Elm$Parser$Layout$layout, $stil4m$elm_syntax$Elm$Parser$Tokens$asToken));
	var parseAsDefinition = function (mod) {
		return $stil4m$elm_syntax$Combine$choice(
			_List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Combine$andThen,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Maybe$Just,
						parseExposingDefinition(mod)),
					A2($stil4m$elm_syntax$Combine$ignore, $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout, asDefinition)),
					A2(parseExposingDefinition, mod, $elm$core$Maybe$Nothing)
				]));
	};
	return A2(
		$stil4m$elm_syntax$Combine$andThen,
		function (_v0) {
			var start = _v0.a;
			return A2(
				$stil4m$elm_syntax$Combine$map,
				$stil4m$elm_syntax$Elm$Parser$Imports$setupNode(start),
				A2(
					$stil4m$elm_syntax$Combine$andThen,
					parseAsDefinition,
					A2($stil4m$elm_syntax$Combine$ignore, $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout, importAndModuleName)));
		},
		$stil4m$elm_syntax$Elm$Parser$Node$parser(
			$stil4m$elm_syntax$Combine$succeed(0)));
}();
var $stil4m$elm_syntax$Elm$Syntax$Module$EffectModule = function (a) {
	return {$: 2, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClause = A2(
	$stil4m$elm_syntax$Combine$andMap,
	A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Tokens$typeName),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
			$stil4m$elm_syntax$Combine$string('='))),
	A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$Tokens$functionName,
		$stil4m$elm_syntax$Combine$succeed($elm$core$Tuple$pair)));
var $stil4m$elm_syntax$Elm$Parser$Modules$whereBlock = A2(
	$stil4m$elm_syntax$Combine$map,
	function (pairs) {
		return {
			bf: A2(
				$elm$core$Maybe$map,
				$elm$core$Tuple$second,
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$first,
							$elm$core$Basics$eq('command')),
						pairs))),
			bV: A2(
				$elm$core$Maybe$map,
				$elm$core$Tuple$second,
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$first,
							$elm$core$Basics$eq('subscription')),
						pairs)))
		};
	},
	A3(
		$stil4m$elm_syntax$Combine$between,
		$stil4m$elm_syntax$Combine$string('{'),
		$stil4m$elm_syntax$Combine$string('}'),
		A2(
			$stil4m$elm_syntax$Combine$sepBy1,
			$stil4m$elm_syntax$Combine$string(','),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides($stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClause))));
var $stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClauses = A2(
	$stil4m$elm_syntax$Combine$continueWith,
	$stil4m$elm_syntax$Elm$Parser$Modules$whereBlock,
	A2(
		$stil4m$elm_syntax$Combine$continueWith,
		$stil4m$elm_syntax$Elm$Parser$Layout$layout,
		$stil4m$elm_syntax$Combine$string('where')));
var $stil4m$elm_syntax$Elm$Parser$Tokens$moduleToken = $stil4m$elm_syntax$Combine$string('module');
var $stil4m$elm_syntax$Elm$Parser$Modules$effectModuleDefinition = function () {
	var createEffectModule = F3(
		function (name, whereClauses, exp) {
			return $stil4m$elm_syntax$Elm$Syntax$Module$EffectModule(
				{bf: whereClauses.bf, ew: exp, bs: name, bV: whereClauses.bV});
		});
	return A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition),
		A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Elm$Parser$Layout$layout,
			A2(
				$stil4m$elm_syntax$Combine$andMap,
				$stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClauses,
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Elm$Parser$Layout$layout,
					A2(
						$stil4m$elm_syntax$Combine$andMap,
						$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Base$moduleName),
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Elm$Parser$Layout$layout,
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Elm$Parser$Tokens$moduleToken,
								A2(
									$stil4m$elm_syntax$Combine$ignore,
									$stil4m$elm_syntax$Elm$Parser$Layout$layout,
									A2(
										$stil4m$elm_syntax$Combine$ignore,
										$stil4m$elm_syntax$Combine$string('effect'),
										$stil4m$elm_syntax$Combine$succeed(createEffectModule))))))))));
}();
var $stil4m$elm_syntax$Elm$Syntax$Module$DefaultModuleData = F2(
	function (moduleName, exposingList) {
		return {ew: exposingList, bs: moduleName};
	});
var $stil4m$elm_syntax$Elm$Syntax$Module$NormalModule = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Modules$normalModuleDefinition = A2(
	$stil4m$elm_syntax$Combine$map,
	$stil4m$elm_syntax$Elm$Syntax$Module$NormalModule,
	A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition),
		A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Elm$Parser$Layout$layout,
			A2(
				$stil4m$elm_syntax$Combine$andMap,
				$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Base$moduleName),
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Elm$Parser$Layout$layout,
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Elm$Parser$Tokens$moduleToken,
						$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Module$DefaultModuleData)))))));
var $stil4m$elm_syntax$Elm$Syntax$Module$PortModule = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Modules$portModuleDefinition = A2(
	$stil4m$elm_syntax$Combine$map,
	$stil4m$elm_syntax$Elm$Syntax$Module$PortModule,
	A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition),
		A2(
			$stil4m$elm_syntax$Combine$ignore,
			$stil4m$elm_syntax$Elm$Parser$Layout$layout,
			A2(
				$stil4m$elm_syntax$Combine$andMap,
				$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Base$moduleName),
				A2(
					$stil4m$elm_syntax$Combine$ignore,
					$stil4m$elm_syntax$Elm$Parser$Layout$layout,
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Elm$Parser$Tokens$moduleToken,
						A2(
							$stil4m$elm_syntax$Combine$ignore,
							$stil4m$elm_syntax$Elm$Parser$Layout$layout,
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Elm$Parser$Tokens$portToken,
								$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$Module$DefaultModuleData)))))))));
var $stil4m$elm_syntax$Elm$Parser$Modules$moduleDefinition = $stil4m$elm_syntax$Combine$choice(
	_List_fromArray(
		[$stil4m$elm_syntax$Elm$Parser$Modules$normalModuleDefinition, $stil4m$elm_syntax$Elm$Parser$Modules$portModuleDefinition, $stil4m$elm_syntax$Elm$Parser$Modules$effectModuleDefinition]));
var $stil4m$elm_syntax$Elm$Parser$File$file = A2(
	$stil4m$elm_syntax$Combine$ignore,
	$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
	A2(
		$stil4m$elm_syntax$Combine$andMap,
		$stil4m$elm_syntax$Elm$Parser$File$collectComments,
		A2(
			$stil4m$elm_syntax$Combine$andMap,
			$stil4m$elm_syntax$Elm$Parser$File$fileDeclarations,
			A2(
				$stil4m$elm_syntax$Combine$ignore,
				$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict),
				A2(
					$stil4m$elm_syntax$Combine$andMap,
					$stil4m$elm_syntax$Combine$many(
						A2($stil4m$elm_syntax$Combine$ignore, $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout, $stil4m$elm_syntax$Elm$Parser$Imports$importDefinition)),
					A2(
						$stil4m$elm_syntax$Combine$ignore,
						$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict),
						A2(
							$stil4m$elm_syntax$Combine$andMap,
							$stil4m$elm_syntax$Elm$Parser$Node$parser($stil4m$elm_syntax$Elm$Parser$Modules$moduleDefinition),
							A2(
								$stil4m$elm_syntax$Combine$ignore,
								$stil4m$elm_syntax$Combine$maybe($stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict),
								$stil4m$elm_syntax$Combine$succeed($stil4m$elm_syntax$Elm$Syntax$File$File)))))))));
var $stil4m$elm_syntax$Elm$Internal$RawFile$Raw = $elm$core$Basics$identity;
var $stil4m$elm_syntax$Elm$Internal$RawFile$fromFile = $elm$core$Basics$identity;
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {eb: col, fx: problem, fD: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.fD, p.eb, p.fx);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0;
		var _v1 = parse(
			{eb: 1, f: _List_Nil, j: 1, b: 0, fD: 1, a: src});
		if (!_v1.$) {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (!_v0.$) {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $stil4m$elm_syntax$Combine$runParser = F3(
	function (_v0, st, s) {
		var p = _v0;
		return A2(
			$elm$parser$Parser$run,
			p(st),
			s);
	});
var $elm$parser$Parser$ExpectingEnd = {$: 10};
var $elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			$elm$core$String$length(s.a),
			s.b) ? A3($elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $stil4m$elm_syntax$Combine$end = function (state) {
	return A2(
		$elm$parser$Parser$map,
		function (x) {
			return _Utils_Tuple2(state, x);
		},
		$elm$parser$Parser$end);
};
var $stil4m$elm_syntax$Elm$Parser$withEnd = function (p) {
	return A2(
		$stil4m$elm_syntax$Combine$ignore,
		$stil4m$elm_syntax$Combine$withLocation(
			function (_v0) {
				return $stil4m$elm_syntax$Combine$end;
			}),
		p);
};
var $stil4m$elm_syntax$Elm$Parser$parse = function (input) {
	var _v0 = A3(
		$stil4m$elm_syntax$Combine$runParser,
		$stil4m$elm_syntax$Elm$Parser$withEnd($stil4m$elm_syntax$Elm$Parser$File$file),
		$stil4m$elm_syntax$Elm$Parser$State$emptyState,
		input + '\n');
	if (!_v0.$) {
		var _v1 = _v0.a;
		var r = _v1.b;
		return $elm$core$Result$Ok(
			$stil4m$elm_syntax$Elm$Internal$RawFile$fromFile(r));
	} else {
		var s = _v0.a;
		return $elm$core$Result$Err(s);
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$isEmpty(dict);
};
var $author$project$Morphir$Graph$reachableNodes = F2(
	function (startNodes, _v0) {
		var edges = _v0;
		var directlyReachable = function (fromNodes) {
			return A3(
				$elm$core$List$foldl,
				$elm$core$Set$union,
				$elm$core$Set$empty,
				A2(
					$elm$core$List$filterMap,
					function (_v1) {
						var fromNode = _v1.b;
						var toNodes = _v1.c;
						return A2($elm$core$Set$member, fromNode, fromNodes) ? $elm$core$Maybe$Just(toNodes) : $elm$core$Maybe$Nothing;
					},
					edges));
		};
		var transitivelyReachable = function (fromNodes) {
			if ($elm$core$Set$isEmpty(fromNodes)) {
				return $elm$core$Set$empty;
			} else {
				var reachables = A2(
					$elm$core$Set$union,
					directlyReachable(fromNodes),
					fromNodes);
				return _Utils_eq(reachables, fromNodes) ? fromNodes : A2(
					$elm$core$Set$union,
					fromNodes,
					transitivelyReachable(reachables));
			}
		};
		return transitivelyReachable(startNodes);
	});
var $elm$core$Set$diff = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$diff, dict1, dict2);
	});
var $author$project$Morphir$Graph$topologicalSort = function (_v0) {
	var edges = _v0;
	var step = F2(
		function (graphEdges, sorting) {
			step:
			while (true) {
				var toNodes = A3(
					$elm$core$List$foldl,
					$elm$core$Set$union,
					$elm$core$Set$empty,
					A2(
						$elm$core$List$map,
						function (_v4) {
							var toKeys = _v4.c;
							return toKeys;
						},
						graphEdges));
				var fromNodes = $elm$core$Set$fromList(
					A2(
						$elm$core$List$map,
						function (_v3) {
							var fromKey = _v3.b;
							return fromKey;
						},
						graphEdges));
				var startNodes = A2($elm$core$Set$diff, fromNodes, toNodes);
				var _v1 = $elm$core$List$head(
					$elm$core$Set$toList(startNodes));
				if (!_v1.$) {
					var startNode = _v1.a;
					var newGraphEdges = A2(
						$elm$core$List$filter,
						function (_v2) {
							var fromKey = _v2.b;
							return !_Utils_eq(fromKey, startNode);
						},
						graphEdges);
					var $temp$graphEdges = newGraphEdges,
						$temp$sorting = A2($elm$core$List$cons, startNode, sorting);
					graphEdges = $temp$graphEdges;
					sorting = $temp$sorting;
					continue step;
				} else {
					return _Utils_Tuple2(
						$elm$core$List$reverse(sorting),
						graphEdges);
				}
			}
		});
	var normalize = function (graphEdges) {
		var toNodes = A3(
			$elm$core$List$foldl,
			$elm$core$Set$union,
			$elm$core$Set$empty,
			A2(
				$elm$core$List$map,
				function (_v7) {
					var toKeys = _v7.c;
					return toKeys;
				},
				graphEdges));
		var fromNodes = $elm$core$Set$fromList(
			A2(
				$elm$core$List$map,
				function (_v6) {
					var fromKey = _v6.b;
					return fromKey;
				},
				graphEdges));
		var emptyFromNodes = A2(
			$elm$core$List$concatMap,
			function (fromKey) {
				return A2(
					$elm$core$List$filterMap,
					function (_v5) {
						var node = _v5.a;
						var key = _v5.b;
						return _Utils_eq(key, fromKey) ? $elm$core$Maybe$Just(
							_Utils_Tuple3(node, fromKey, $elm$core$Set$empty)) : $elm$core$Maybe$Nothing;
					},
					graphEdges);
			},
			$elm$core$Set$toList(
				A2($elm$core$Set$diff, toNodes, fromNodes)));
		return _Utils_ap(graphEdges, emptyFromNodes);
	};
	return A2(
		step,
		normalize(edges),
		_List_Nil);
};
var $author$project$Morphir$Elm$Frontend$packageDefinitionFromSource = F3(
	function (packageInfo, dependencies, sourceFiles) {
		var sortModules = function (modules) {
			var _v3 = $author$project$Morphir$Graph$topologicalSort(
				$author$project$Morphir$Graph$fromList(
					A2(
						$elm$core$List$map,
						function (_v4) {
							var moduleName = _v4.a;
							var parsedFile = _v4.b;
							return _Utils_Tuple3(
								0,
								moduleName,
								A2(
									$elm$core$List$map,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.bs;
										},
										$stil4m$elm_syntax$Elm$Syntax$Node$value),
									$stil4m$elm_syntax$Elm$RawFile$imports(parsedFile.aN)));
						},
						modules)));
			var sortedModules = _v3.a;
			var cycles = _v3.b;
			return $author$project$Morphir$Graph$isEmpty(cycles) ? $elm$core$Result$Ok(
				$elm$core$List$reverse(sortedModules)) : $elm$core$Result$Err(
				_List_fromArray(
					[
						$author$project$Morphir$Elm$Frontend$CyclicModules(cycles)
					]));
		};
		var parseSources = function (sources) {
			return $author$project$Morphir$ListOfResults$liftAllErrors(
				A2(
					$elm$core$List$map,
					function (sourceFile) {
						return A2(
							$elm$core$Result$mapError,
							$author$project$Morphir$Elm$Frontend$ParseError(sourceFile.fw),
							A2(
								$elm$core$Result$map,
								function (rawFile) {
									return _Utils_Tuple2(
										$stil4m$elm_syntax$Elm$RawFile$moduleName(rawFile),
										A2($author$project$Morphir$Elm$Frontend$ParsedFile, sourceFile, rawFile));
								},
								$stil4m$elm_syntax$Elm$Parser$parse(sourceFile.ee)));
					},
					sources));
		};
		var exposedModuleNames = A2(
			$elm$core$Set$map,
			function (modulePath) {
				return A2(
					$elm$core$List$map,
					$author$project$Morphir$IR$Name$toTitleCase,
					_Utils_ap(
						$author$project$Morphir$IR$Path$toList(packageInfo.cN),
						$author$project$Morphir$IR$Path$toList(modulePath)));
			},
			packageInfo.bj);
		var treeShakeModules = function (allModules) {
			var allUsedModules = A2(
				$author$project$Morphir$Graph$reachableNodes,
				exposedModuleNames,
				$author$project$Morphir$Graph$fromList(
					A2(
						$elm$core$List$map,
						function (_v2) {
							var moduleName = _v2.a;
							var parsedFile = _v2.b;
							return _Utils_Tuple3(
								0,
								moduleName,
								A2(
									$elm$core$List$map,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.bs;
										},
										$stil4m$elm_syntax$Elm$Syntax$Node$value),
									$stil4m$elm_syntax$Elm$RawFile$imports(parsedFile.aN)));
						},
						allModules)));
			return A2(
				$elm$core$List$filter,
				function (_v1) {
					var moduleName = _v1.a;
					return A2($elm$core$Set$member, moduleName, allUsedModules);
				},
				allModules);
		};
		return A2(
			$elm$core$Result$map,
			function (moduleDefs) {
				return {
					e7: $elm$core$Dict$fromList(
						A2(
							$elm$core$List$map,
							function (_v0) {
								var modulePath = _v0.a;
								var m = _v0.b;
								return A2($elm$core$Set$member, modulePath, packageInfo.bj) ? _Utils_Tuple2(
									modulePath,
									$author$project$Morphir$IR$AccessControlled$public(m)) : _Utils_Tuple2(
									modulePath,
									$author$project$Morphir$IR$AccessControlled$private(m));
							},
							$elm$core$Dict$toList(moduleDefs)))
				};
			},
			A2(
				$elm$core$Result$andThen,
				function (parsedFiles) {
					var parsedFilesByModuleName = $elm$core$Dict$fromList(parsedFiles);
					return A2(
						$elm$core$Result$andThen,
						A3($author$project$Morphir$Elm$Frontend$mapParsedFiles, dependencies, packageInfo.cN, parsedFilesByModuleName),
						sortModules(
							treeShakeModules(parsedFiles)));
				},
				parseSources(sourceFiles)));
	});
var $author$project$Morphir$Elm$Frontend$mapSource = F3(
	function (packageInfo, dependencies, sourceFiles) {
		var mapParserProblem = function (problem) {
			switch (problem.$) {
				case 0:
					var something = problem.a;
					return 'Expecting ' + something;
				case 1:
					return 'Expecting integer';
				case 2:
					return 'Expecting hexadecimal';
				case 3:
					return 'Expecting octal';
				case 4:
					return 'Expecting binary';
				case 5:
					return 'Expecting float';
				case 6:
					return 'Expecting number';
				case 7:
					return 'Expecting variable';
				case 8:
					var symbol = problem.a;
					return 'Expecting symbol: ' + symbol;
				case 9:
					var keyword = problem.a;
					return 'Expecting keyword: ' + keyword;
				case 10:
					return 'Expecting end';
				case 11:
					return 'Unexpected character';
				case 12:
					var message = problem.a;
					return 'Problem: ' + message;
				default:
					return 'Bad repeat';
			}
		};
		var mapParserDeadEnd = function (deadEnd) {
			var location = {ec: deadEnd.eb, fD: deadEnd.fD};
			return {
				er: 'Parse error: ' + mapParserProblem(deadEnd.fx),
				fP: _List_fromArray(
					[
						{eq: location, fT: location}
					])
			};
		};
		var mapContentRange = function (contentRange) {
			return contentRange;
		};
		var mapSourceLocations = F3(
			function (message, sourceLocation, moreSourceLocations) {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						sourceLocation.de.fw,
						{
							er: message,
							fP: A2(
								$elm$core$List$map,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.c$;
									},
									mapContentRange),
								A2($elm$core$List$cons, sourceLocation, moreSourceLocations))
						})
					]);
			});
		return A2(
			$elm$core$Result$mapError,
			function (errors) {
				var globalErrors = A2(
					$elm$core$List$filterMap,
					function (error) {
						if (error.$ === 1) {
							var graph = error.a;
							return $elm$core$Maybe$Just(
								$author$project$Morphir$Compiler$ErrorAcrossSourceFiles(
									{
										er: 'Module imports form a cycle',
										eC: A2(
											$elm$core$List$map,
											$elm$core$String$join('/'),
											$elm$core$Set$toList(
												$author$project$Morphir$Graph$nodeLabels(graph)))
									}));
						} else {
							return $elm$core$Maybe$Nothing;
						}
					},
					errors);
				var fileSpecificErrors = A2(
					$elm$core$List$map,
					function (_v3) {
						var filePath = _v3.a;
						var fileErrors = _v3.b;
						return A2($author$project$Morphir$Compiler$ErrorsInSourceFile, filePath, fileErrors);
					},
					$elm$core$Dict$toList(
						A3(
							$elm$core$List$foldl,
							F2(
								function (_v1, soFar) {
									var filePath = _v1.a;
									var fileError = _v1.b;
									return A3(
										$elm$core$Dict$update,
										filePath,
										function (maybeErrorsSoFar) {
											if (!maybeErrorsSoFar.$) {
												var errorsSoFar = maybeErrorsSoFar.a;
												return $elm$core$Maybe$Just(
													A2($elm$core$List$cons, fileError, errorsSoFar));
											} else {
												return $elm$core$Maybe$Just(
													_List_fromArray(
														[fileError]));
											}
										},
										soFar);
								}),
							$elm$core$Dict$empty,
							A2(
								$elm$core$List$concatMap,
								function (error) {
									switch (error.$) {
										case 0:
											var filePath = error.a;
											var deadEnds = error.b;
											return A2(
												$elm$core$List$map,
												A2(
													$elm$core$Basics$composeR,
													mapParserDeadEnd,
													$elm$core$Tuple$pair(filePath)),
												deadEnds);
										case 1:
											return _List_Nil;
										case 2:
											var sourceLocation = error.a;
											var message = error.b;
											return A3(
												mapSourceLocations,
												'Resolve error: ' + $author$project$Morphir$Elm$Frontend$Resolve$errorToMessage(message),
												sourceLocation,
												_List_Nil);
										case 3:
											var sourceLocation = error.a;
											return A3(mapSourceLocations, 'Empty apply', sourceLocation, _List_Nil);
										case 4:
											var sourceLocation = error.a;
											var string = error.b;
											return A3(mapSourceLocations, 'Not supported: ' + string, sourceLocation, _List_Nil);
										case 5:
											var name = error.a;
											var sourceLocation = error.b;
											var sourceLocation2 = error.c;
											return A3(
												mapSourceLocations,
												'Duplicate name in pattern: ' + $author$project$Morphir$IR$Name$toCamelCase(name),
												sourceLocation,
												_List_fromArray(
													[sourceLocation2]));
										case 6:
											var name = error.a;
											var sourceLocation = error.b;
											var sourceLocation2 = error.c;
											return A3(
												mapSourceLocations,
												'Variable shadowing: ' + $author$project$Morphir$IR$Name$toCamelCase(name),
												sourceLocation,
												_List_fromArray(
													[sourceLocation2]));
										case 7:
											var sourceLocation = error.a;
											return A3(mapSourceLocations, 'Missing type signature', sourceLocation, _List_Nil);
										default:
											var sourceLocation = error.a;
											return A3(mapSourceLocations, 'Record pattern not supported', sourceLocation, _List_Nil);
									}
								},
								errors))));
				return _Utils_ap(fileSpecificErrors, globalErrors);
			},
			A3($author$project$Morphir$Elm$Frontend$packageDefinitionFromSource, packageInfo, dependencies, sourceFiles));
	});
var $author$project$Morphir$IR$Package$mapSpecificationAttributes = F2(
	function (tf, spec) {
		return $author$project$Morphir$IR$Package$Specification(
			A2(
				$elm$core$Dict$map,
				F2(
					function (_v0, moduleSpec) {
						return A2($author$project$Morphir$IR$Module$mapSpecificationAttributes, tf, moduleSpec);
					}),
				spec.e7));
	});
var $author$project$Morphir$Elm$CLI$packageDefinitionFromSourceResult = _Platform_outgoingPort('packageDefinitionFromSourceResult', $elm$core$Basics$identity);
var $author$project$Morphir$Elm$CLI$update = F2(
	function (msg, model) {
		if (!msg.$) {
			var _v1 = msg.a;
			var packageInfoJson = _v1.a;
			var sourceFiles = _v1.b;
			var _v2 = A2($elm$json$Json$Decode$decodeValue, $author$project$Morphir$Elm$Frontend$Codec$decodePackageInfo, packageInfoJson);
			if (!_v2.$) {
				var packageInfo = _v2.a;
				var frontendResult = A3($author$project$Morphir$Elm$Frontend$mapSource, packageInfo, $elm$core$Dict$empty, sourceFiles);
				var typedResult = A2(
					$elm$core$Result$andThen,
					function (packageDef) {
						var thisPackageSpec = A2(
							$author$project$Morphir$IR$Package$mapSpecificationAttributes,
							function (_v5) {
								return 0;
							},
							$author$project$Morphir$IR$Package$definitionToSpecificationWithPrivate(packageDef));
						var references = A3($elm$core$Dict$insert, packageInfo.cN, thisPackageSpec, $author$project$Morphir$Elm$Frontend$defaultDependencies);
						return A2(
							$author$project$Morphir$Type$Infer$inferPackageDefinition,
							references,
							A3(
								$author$project$Morphir$IR$Package$mapDefinitionAttributes,
								function (_v4) {
									return 0;
								},
								$elm$core$Basics$identity,
								packageDef));
					},
					frontendResult);
				return _Utils_Tuple2(
					model,
					$author$project$Morphir$Elm$CLI$packageDefinitionFromSourceResult(
						A3(
							$author$project$Morphir$Elm$CLI$encodeResult,
							$elm$json$Json$Encode$list($author$project$Morphir$Compiler$Codec$encodeError),
							$author$project$Morphir$IR$Distribution$Codec$encodeDistribution,
							A2(
								$elm$core$Result$map,
								A2($author$project$Morphir$IR$Distribution$Library, packageInfo.cN, $elm$core$Dict$empty),
								A2(
									$elm$core$Result$map,
									A2(
										$author$project$Morphir$IR$Package$mapDefinitionAttributes,
										$elm$core$Basics$identity,
										function (_v3) {
											var tpe = _v3.b;
											return tpe;
										}),
									typedResult)))));
			} else {
				var errorMessage = _v2.a;
				return _Utils_Tuple2(
					model,
					$author$project$Morphir$Elm$CLI$jsonDecodeError(
						$elm$json$Json$Decode$errorToString(errorMessage)));
			}
		} else {
			var _v6 = msg.a;
			var optionsJson = _v6.a;
			var packageDistJson = _v6.b;
			var targetOption = A2(
				$elm$json$Json$Decode$decodeValue,
				A2($elm$json$Json$Decode$field, 'target', $elm$json$Json$Decode$string),
				optionsJson);
			var packageDistroResult = A2($elm$json$Json$Decode$decodeValue, $author$project$Morphir$IR$Distribution$Codec$decodeDistribution, packageDistJson);
			var optionsResult = A2(
				$elm$json$Json$Decode$decodeValue,
				$author$project$Morphir$Elm$Target$decodeOptions(targetOption),
				optionsJson);
			var _v7 = A3($elm$core$Result$map2, $elm$core$Tuple$pair, optionsResult, packageDistroResult);
			if (!_v7.$) {
				var _v8 = _v7.a;
				var options = _v8.a;
				var packageDist = _v8.b;
				var enrichedDistro = function () {
					var packageName = packageDist.a;
					var dependencies = packageDist.b;
					var packageDef = packageDist.c;
					return A3(
						$author$project$Morphir$IR$Distribution$Library,
						packageName,
						A2($elm$core$Dict$union, $author$project$Morphir$Elm$Frontend$defaultDependencies, dependencies),
						packageDef);
				}();
				var fileMap = A2($author$project$Morphir$Elm$Target$mapDistribution, options, enrichedDistro);
				return _Utils_Tuple2(
					model,
					$author$project$Morphir$Elm$CLI$generateResult(
						A3(
							$author$project$Morphir$Elm$CLI$encodeResult,
							$elm$json$Json$Encode$string,
							$author$project$Morphir$File$FileMap$Codec$encodeFileMap,
							$elm$core$Result$Ok(fileMap))));
			} else {
				var errorMessage = _v7.a;
				return _Utils_Tuple2(
					model,
					$author$project$Morphir$Elm$CLI$jsonDecodeError(
						$elm$json$Json$Decode$errorToString(errorMessage)));
			}
		}
	});
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Morphir$Elm$CLI$main = $elm$core$Platform$worker(
	{
		eP: function (_v0) {
			return _Utils_Tuple2(0, $elm$core$Platform$Cmd$none);
		},
		f$: $author$project$Morphir$Elm$CLI$subscriptions,
		gp: $author$project$Morphir$Elm$CLI$update
	});
_Platform_export({'Morphir':{'Elm':{'CLI':{'init':$author$project$Morphir$Elm$CLI$main(
	$elm$json$Json$Decode$succeed(0))(0)}}}});}(this));