'use strict';

// 15.3.4.5 Function.prototype.bind (thisArg [, arg1 [, arg2, …]])
// TODO: Use Array.prototype.slice.
Object.defineProperty(Function.prototype, 'bind', {
	value: function (boundThis) {
		if (!(this instanceof Function)) {
			throw new TypeError('Target must be a function');
		}
		
		var target = this;
		var boundArgs = [];
		for (var i = 1; i < arguments.length; i++) {
			boundArgs.push(arguments[i]);
		}
		
		var result = function () {
			var args = [];
			for (var i = 0; i < boundArgs.length; i++) {
				args.push(boundArgs[i]);
			}
			for (var i = 0; i < arguments.lenngth; i++) {
				args.push(arguments[i]);
			}
			return target.apply(boundThis, args);
		};
		
		var thrower = function () {
			throw new TypeError('Cannot access caller or arguments on bound function');
		};
		
		Object.defineProperty(result, 'caller', {
			get: thrower,
			set: thrower,
			enumerable: false,
			configurable: false
		});
		Object.defineProperty(result, 'arguments', {
			get: thrower,
			set: thrower,
			enumerable: false,
			configurable: false
		});
		Object.defineProperty(result, 'length', {
			value: target.length - boundArgs.length,
			writable: false,
			enumerable: false,
			configurable: false
		});
		
		return result;
	},
	writable: true,
	enumerable: false,
	configurable: true
});
