// http://ecma-international.org/ecma-262/5.1/#sec-15.11

function Error(message) {
	if (!(this instanceof Error)) {
		return new Error(message);
	}
	
	this.message = message;
}

Error.prototype.name = 'Error';

Error.prototype.message = '';

Error.prototype.toString = function () {
	if (typeof this != 'object') {
		throw new TypeError();
	}
	
	var name = this.name;
	name = name === undefined ? 'Error' : name.toString();
	
	var msg = this.message;
	msg = msg === undefined ? '' : msg.toString();
	
	if (name === '') {
		return msg;
	}
	if (msg === '') {
		return name;
	}
	
	return name + ': ' + msg;
}

// http://ecma-international.org/ecma-262/5.1/#sec-15.11.6.1

function EvalError(message) {
	if (!(this instanceof EvalError)) {
		return new EvalError(message);
	}
	
	Error.call(this, message);
}

EvalError.prototype = Object.create(Error.prototype);

EvalError.prototype.name = 'EvalError';

EvalError.prototype.constructor = EvalError;

// http://ecma-international.org/ecma-262/5.1/#sec-15.11.6.2

function RangeError(message) {
	if (!(this instanceof RangeError)) {
		return new RangeError(message);
	}
	
	Error.call(this, message);
}

RangeError.prototype = Object.create(Error.prototype);

RangeError.prototype.name = 'RangeError';

RangeError.prototype.constructor = RangeError;

//http://ecma-international.org/ecma-262/5.1/#sec-15.11.6.3

function ReferenceError(message) {
	if (!(this instanceof ReferenceError)) {
		return new ReferenceError(message);
	}
	
	Error.call(this, message);
}

ReferenceError.prototype = Object.create(Error.prototype);

ReferenceError.prototype.name = 'ReferenceError';

ReferenceError.prototype.constructor = ReferenceError;

//http://ecma-international.org/ecma-262/5.1/#sec-15.11.6.4

function SyntaxError(message) {
	if (!(this instanceof SyntaxError)) {
		return new SyntaxError(message);
	}
	
	Error.call(this, message);
}

SyntaxError.prototype = Object.create(Error.prototype);

SyntaxError.prototype.name = 'SyntaxError';

SyntaxError.prototype.constructor = SyntaxError;

//http://ecma-international.org/ecma-262/5.1/#sec-15.11.6.5

function TypeError(message) {
	if (!(this instanceof TypeError)) {
		return new TypeError(message);
	}
	
	Error.call(this, message);
}

TypeError.prototype = Object.create(Error.prototype);

TypeError.prototype.name = 'TypeError';

TypeError.prototype.constructor = TypeError;

//http://ecma-international.org/ecma-262/5.1/#sec-15.11.6.6

function URIError(message) {
	if (!(this instanceof URIError)) {
		return new URIError(message);
	}
	
	Error.call(this, message);
}

URIError.prototype = Object.create(Error.prototype);

URIError.prototype.name = 'URIError';

URIError.prototype.constructor = URIError;
