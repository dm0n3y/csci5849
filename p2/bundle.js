(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Parser = Parser;
exports.toValue = exports.toPromise = exports.takeLeft = exports.takeRight = exports.recursiveParser = exports.whitespace = exports.endOfInput = exports.skip = exports.possibly = exports.lookAhead = exports.anythingExcept = exports.everythingUntil = exports.between = exports.choice = exports.sepBy1 = exports.sepBy = exports.sequenceOf = exports.namedSequenceOf = exports.anyOfString = exports.letters = exports.letter = exports.digits = exports.digit = exports.regex = exports.str = exports.char = exports.leftMapTo = exports.mapTo = exports.many1 = exports.many = exports.succeedWith = exports.fail = exports.decide = exports.parse = exports.tapParser = exports.composeParsers = exports.pipeParsers = void 0;

var _data = require("data.either");

function Parser(p) {
  this.p = p;
}

;

Parser.prototype.run = function Parser$run(targetString) {
  return this.p((0, _data.Right)([0, targetString, null])).map(result => {
    return result[2];
  });
};

Parser.prototype['fantasy-land/map'] = function Parser$map(fn) {
  const that = this;
  return new Parser(function Parser$map$state(state) {
    return that.p(state).map(function Parser$map$state$map([i, s, v]) {
      return [i, s, fn(v)];
    });
  });
};

Parser.prototype['fantasy-land/chain'] = function Parser$chain(fn) {
  const that = this;
  return new Parser(function Parser$chain$state(state) {
    return that.p(state).chain(function Parser$chain$chain([i, s, v]) {
      return fn(v).p((0, _data.Right)([i, s, v]));
    });
  });
};

Parser.prototype['fantasy-land/ap'] = function Parser$ap(parserOfFunction) {
  const that = this;
  return new Parser(function Parser$ap$state(state) {
    return parserOfFunction.p(state).chain(function Parser$ap$chain([_, __, fn]) {
      return that.p(state).map(function Parser$ap$chain$map([i, s, v]) {
        return [i, s, fn(v)];
      });
    });
  });
};

Parser.prototype.leftMap = function Parser$leftMap(fn) {
  const that = this;
  return new Parser(function Parser$leftMap$state(state) {
    return that.p(state).leftMap(function Parser$leftMap$state$leftMap([i, e]) {
      return [i, fn(e, i)];
    });
  });
};

Parser.prototype.map = Parser.prototype['fantasy-land/map'];
Parser.prototype.ap = Parser.prototype['fantasy-land/ap'];
Parser.prototype.chain = Parser.prototype['fantasy-land/chain'];

Parser['fantasy-land/of'] = function (x) {
  return new Parser(state => {
    return state.map(([i, s]) => [i, s, x]);
  });
};

Parser.of = Parser['fantasy-land/of']; //           pipeParsers :: [Parser * * *] -> Parser * * *

const pipeParsers = function pipeParsers(parsers) {
  return new Parser(function pipeParsers$state(state) {
    let nextState = state;

    for (const parser of parsers) {
      nextState = parser.p(nextState);
    }

    return nextState;
  });
}; //           composeParsers :: [Parser * * *] -> Parser * * *


exports.pipeParsers = pipeParsers;

const composeParsers = function composeParsers(parsers) {
  return new Parser(function composeParsers$state(state) {
    return pipeParsers([...parsers].reverse()).p(state);
  });
}; //           tapParser :: (a => void) -> Parser e a a


exports.composeParsers = composeParsers;

const tapParser = function tapParser(fn) {
  return new Parser(function tapParser$state(state) {
    fn(state.value);
    return state;
  });
}; //           parse :: Parser e a b -> String -> Either e b


exports.tapParser = tapParser;

const parse = function parse(parser) {
  return function parse$targetString(targetString) {
    return parser.run(targetString);
  };
}; //           decide :: (a -> Parser e b c) -> Parser e b c


exports.parse = parse;

const decide = function decide(fn) {
  return new Parser(function decide$state(state) {
    return state.chain(function decide$state$chain([_, __, v]) {
      const parser = fn(v);
      return parser.p(state);
    });
  });
}; //           fail :: String -> Parser String a b


exports.decide = decide;

const fail = function fail(errorMessage) {
  return new Parser(function fail$state(state) {
    return state.chain(function fail$state$chain([i]) {
      return (0, _data.Left)([i, errorMessage]);
    });
  });
}; //           succeedWith :: b -> Parser e a b


exports.fail = fail;

const succeedWith = function succeedWith(x) {
  return new Parser(function succeedWith$state(state) {
    return state.map(function succeedWith$state$map([i, s]) {
      return [i, s, x];
    });
  });
}; //           many :: Parser e a b -> Parser e a [b]


exports.succeedWith = succeedWith;

const many = function many(parser) {
  return new Parser(function many$state(state) {
    return state.chain(function many$state$chain(innerState) {
      const results = [];
      let nextState = innerState;

      while (true) {
        const out = parser.p((0, _data.Right)(nextState));

        if (out.isLeft) {
          break;
        } else {
          nextState = out.value;
          results.push(nextState[2]);

          if (nextState[0] >= nextState[1].length) {
            break;
          }
        }
      }

      const [index, targetString] = nextState;
      return (0, _data.Right)([index, targetString, results]);
    });
  });
}; //           many1 :: Parser e a b -> Parser String a [b]


exports.many = many;

const many1 = function many1(parser) {
  return new Parser(function many1$state(state) {
    const res = many(parser).p(state);
    return res.chain(function many1$state$chain([index, targetString, value]) {
      if (value.length === 0) {
        return (0, _data.Left)([index, `ParseError 'many1' (position ${index}): Expecting to match at least one value`]);
      }

      return (0, _data.Right)([index, targetString, value]);
    });
  });
}; //           mapTo :: (a -> b) -> Parser e a b


exports.many1 = many1;

const mapTo = function mapTo(fn) {
  return new Parser(function mapTo$state(state) {
    return state.map(function mapTo$state$map([index, targetString, res]) {
      return [index, targetString, fn(res)];
    });
  });
}; //           leftMapTo :: ((e, Int) -> f) -> Parser f a b


exports.mapTo = mapTo;

const leftMapTo = fn => new Parser(state => {
  return state.leftMap(([index, errorString]) => {
    return [index, fn(errorString, index)];
  });
}); //           char :: Char -> Parser String a String


exports.leftMapTo = leftMapTo;

const char = function char(c) {
  if (!c || c.length !== 1) {
    throw new TypeError(`char must be called with a single character, but got ${c}`);
  }

  return new Parser(function char$state(state) {
    return state.chain(function char$state$chain([index, targetString]) {
      if (index < targetString.length) {
        if (targetString[index] === c) {
          return (0, _data.Right)([index + 1, targetString, c]);
        } else {
          return (0, _data.Left)([index, `ParseError (position ${index}): Expecting character '${c}', got '${targetString[index]}'`]);
        }
      }

      return (0, _data.Left)([index, `ParseError (position ${index}): Expecting character '${c}', but got end of input.`]);
    });
  });
}; //           str :: String -> Parser String a String


exports.char = char;

const str = function str(s) {
  if (!s || s.length < 1) {
    throw new TypeError(`str must be called with a string with length > 1, but got ${s}`);
  }

  return new Parser(function str$state(state) {
    return state.chain(function str$state$chain([index, targetString]) {
      const rest = targetString.slice(index);

      if (rest.length >= 1) {
        if (rest.startsWith(s)) {
          return (0, _data.Right)([index + s.length, targetString, s]);
        } else {
          return (0, _data.Left)([index, `ParseError (position ${index}): Expecting string '${s}', got '${rest.slice(0, s.length)}...'`]);
        }
      }

      return (0, _data.Left)([index, `ParseError (position ${index}): Expecting string '${s}', but got end of input.`]);
    });
  });
}; //           regex :: RegExp -> Parser String a String


exports.str = str;

const regex = function regex(re) {
  const typeofre = Object.prototype.toString.call(re);

  if (typeofre !== '[object RegExp]') {
    throw new TypeError(`regex must be called with a Regular Expression, but got ${typeofre}`);
  }

  if (re.toString()[1] !== '^') {
    throw new Error(`regex parsers must contain '^' start assertion.`);
  }

  return new Parser(function regex$state(state) {
    return state.chain(function regex$state$chain([index, targetString]) {
      const rest = targetString.slice(index);

      if (rest.length >= 1) {
        const match = rest.match(re);

        if (match) {
          return (0, _data.Right)([index + match[0].length, targetString, match[0]]);
        } else {
          return (0, _data.Left)([index, `ParseError (position ${index}): Expecting string matching '${re}', got '${rest.slice(0, 5)}...'`]);
        }
      }

      return (0, _data.Left)([index, `ParseError (position ${index}): Expecting string matching '${re}', but got end of input.`]);
    });
  });
}; //           digit :: Parser String a String


exports.regex = regex;
const digit = new Parser(function digit$state(state) {
  return state.chain(function digit$state$chain([index, targetString]) {
    const rest = targetString.slice(index);

    if (rest.length >= 1) {
      if (/[0-9]/.test(rest[0])) {
        return (0, _data.Right)([index + 1, targetString, rest[0]]);
      } else {
        return (0, _data.Left)([index, `ParseError (position ${index}): Expecting digit, got '${rest[0]}'`]);
      }
    }

    return (0, _data.Left)([index, `ParseError (position ${index}): Expecting digit, but got end of input.`]);
  });
}); //           digits :: Parser String a String

exports.digit = digit;
const digits = many1(digit).map(x => x.join('')); //           letter :: Parser String a String

exports.digits = digits;
const letter = new Parser(function letter$state(state) {
  return state.chain(function letter$state$chain([index, targetString]) {
    const rest = targetString.slice(index);

    if (rest.length >= 1) {
      if (/[a-zA-Z]/.test(rest[0])) {
        return (0, _data.Right)([index + 1, targetString, rest[0]]);
      } else {
        return (0, _data.Left)([index, `ParseError (position ${index}): Expecting letter, got ${rest[0]}`]);
      }
    }

    return (0, _data.Left)([index, `ParseError (position ${index}): Expecting letter, but got end of input.`]);
  });
}); //           letters :: Parser String a String

exports.letter = letter;
const letters = many1(letter).map(x => x.join('')); //           anyOfString :: String -> Parser String a String

exports.letters = letters;

const anyOfString = function anyOfString(s) {
  return new Parser(function anyOfString$state(state) {
    return state.chain(([index, targetString]) => {
      const rest = targetString.slice(index);

      if (rest.length >= 1) {
        if (s.includes(rest[0])) {
          return (0, _data.Right)([index + 1, targetString, rest[0]]);
        } else {
          return (0, _data.Left)([index, `ParseError (position ${index}): Expecting any of the string "${s}", got ${rest[0]}`]);
        }
      }

      return (0, _data.Left)([index, `ParseError (position ${index}): Expecting any of the string "${s}", but got end of input.`]);
    });
  });
}; //           namedSequenceOf :: [(String, Parser e a b)] -> Parser e a (StrMap b)


exports.anyOfString = anyOfString;

const namedSequenceOf = function namedSequenceOf(pairedParsers) {
  return new Parser(function namedSequenceOf$state(state) {
    return state.chain(() => {
      const results = {};
      let nextState = state;

      for (const [key, parser] of pairedParsers) {
        const out = parser.p(nextState);

        if (out.isLeft) {
          return out;
        } else {
          nextState = out;
          results[key] = out.value[2];
        }
      }

      const [i, s] = nextState.value;
      return (0, _data.Right)([i, s, results]);
    });
  });
}; //           sequenceOf :: [Parser e a b] -> Parser e a [b]


exports.namedSequenceOf = namedSequenceOf;

const sequenceOf = function sequenceOf(parsers) {
  return new Parser(function sequenceOf$state(state) {
    return state.chain(() => {
      const results = new Array(parsers.length);
      let nextState = state;

      for (let i = 0; i < parsers.length; i++) {
        const out = parsers[i].p(nextState);

        if (out.isLeft) {
          return out;
        } else {
          nextState = out;
          results[i] = out.value[2];
        }
      }

      const [i, s] = nextState.value;
      return (0, _data.Right)([i, s, results]);
    });
  });
}; //           sepBy :: Parser e a c -> Parser e a b -> Parser e a [b]


exports.sequenceOf = sequenceOf;

const sepBy = function sepBy(sepParser) {
  return function sepBy$valParser(valParser) {
    return new Parser(function sepBy$valParser$state(state) {
      return state.chain(function sepBy$valParser$state$chain() {
        let nextState = state;
        let left = null;
        const results = [];

        while (true) {
          const valState = valParser.p(nextState);
          const sepState = sepParser.p(valState);

          if (valState.isLeft) {
            left = valState;
            break;
          } else {
            results.push(valState.value[2]);
          }

          if (sepState.isLeft) {
            nextState = valState;
            break;
          }

          nextState = sepState;
        }

        if (left) {
          if (results.length === 0) {
            const [i, s] = state.value;
            return (0, _data.Right)([i, s, results]);
          }

          return left;
        }

        const [i, s] = nextState.value;
        return (0, _data.Right)([i, s, results]);
      });
    });
  };
}; //           sepBy1 :: Parser e a c -> Parser f a b  -> Parser String a [b]


exports.sepBy = sepBy;

const sepBy1 = function sepBy1(sepParser) {
  return function sepBy1$valParser(valParser) {
    return new Parser(function sepBy1$valParser$state(state) {
      return sepBy(sepParser)(valParser).p(state).chain(function sepBy1$valParser$state$chain([index, targetString, value]) {
        if (value.length === 0) {
          return (0, _data.Left)([index, `ParseError 'sepBy1' (position ${index}): Expecting to match at least one separated value`]);
        }

        return (0, _data.Right)([index, targetString, value]);
      });
    });
  };
}; //           choice :: [Parser e a *] -> Parser e a *


exports.sepBy1 = sepBy1;

const choice = function choice(parsers) {
  return new Parser(function choice$state(state) {
    let left = null;
    return state.chain(function choice$state$chain() {
      for (const parser of parsers) {
        const out = parser.p(state);

        if (out.isLeft) {
          if (!left || out.value[0] > left.value[0]) {
            left = out;
          }
        } else {
          return out;
        }
      }

      return left;
    });
  });
}; //           between :: Parser e a b -> Parser f a c -> Parser g a d -> Parser g a d


exports.choice = choice;

const between = function between(leftParser) {
  return function between$rightParser(rightParser) {
    return function between$rightParser(parser) {
      return sequenceOf([leftParser, parser, rightParser]).map(([_, x]) => x);
    };
  };
}; //           everythingUntil :: Parser e a b -> Parser String a c


exports.between = between;

const everythingUntil = function everythingUntil(parser) {
  return new Parser(state => {
    return state.chain(function everythingUntil$state(innerState) {
      const results = [];
      let nextState = state;

      while (true) {
        const out = parser.p(nextState);

        if (out.isLeft) {
          const [index, targetString] = nextState.value;
          const val = targetString[index];

          if (val) {
            results.push(val);
            nextState = (0, _data.Right)([index + 1, targetString, val]);
          } else {
            return (0, _data.Left)([nextState[0], `ParseError 'everythingUntil' (position ${nextState.value[0]}): Unexpected end of input.`]);
          }
        } else {
          break;
        }
      }

      const [i, s] = nextState.value;
      return (0, _data.Right)([i, s, results.join('')]);
    });
  });
}; //           anythingExcept :: Parser e a b -> Parser String a c


exports.everythingUntil = everythingUntil;

const anythingExcept = function anythingExcept(parser) {
  return new Parser(function anythingExcept$state(state) {
    return state.chain(([index, targetString]) => {
      const out = parser.p(state);

      if (out.isLeft) {
        return (0, _data.Right)([index + 1, targetString, targetString[index]]);
      }

      return (0, _data.Left)([index, `ParseError 'anythingExcept' (position ${index}): Matched '${out.value[2]}' from the exception parser`]);
    });
  });
}; //           lookAhead :: Parser e a b -> Parser e a b


exports.anythingExcept = anythingExcept;

const lookAhead = function lookAhead(parser) {
  return new Parser(function lookAhead$state(state) {
    return state.chain(function lookAhead$state$chain([i, s]) {
      const nextState = parser.p(state);
      return nextState.map(function lookAhead$state$chain$map([_, __, v]) {
        return [i, s, v];
      });
    });
  });
}; //           possibly :: Parser e a b -> Parser e a (b|null)


exports.lookAhead = lookAhead;

const possibly = function possibly(parser) {
  return new Parser(function possibly$state(state) {
    return state.chain(function possibly$state$chain([index, targetString]) {
      const nextState = parser.p(state);

      if (nextState.isLeft) {
        return (0, _data.Right)([index, targetString, null]);
      }

      return nextState;
    });
  });
}; //           skip :: Parser e a b -> Parser e a a


exports.possibly = possibly;

const skip = function skip(parser) {
  return new Parser(function skip$state(state) {
    return state.chain(function skip$state$chain([_, __, value]) {
      const nextState = parser.p(state);
      return nextState.map(([i, s]) => [i, s, value]);
    });
  });
}; //           endOfInput :: Parser e a b


exports.skip = skip;
const endOfInput = new Parser(function endOfInput$state(state) {
  return state.chain(function endOfInput$state$chain([index, targetString]) {
    if (index !== targetString.length) {
      return (0, _data.Left)([index, `ParseError 'endOfInput' (position ${index}): Expected end of input but got '${targetString.slice(index, index + 1)}'`]);
    }

    return (0, _data.Right)([index, targetString, null]);
  });
}); //           whitespace :: Parser e a String

exports.endOfInput = endOfInput;
const whitespace = many(anyOfString(' \n\t\r')).map(x => x.join('')); //           recursiveParser :: (() => Parser e a b) -> Parser e a b

exports.whitespace = whitespace;

const recursiveParser = function recursiveParser(parserThunk) {
  return new Parser(function recursiveParser$state(state) {
    return parserThunk().p(state);
  });
}; //           takeRight :: Parser e a b -> Parser f b c -> Parser f a c


exports.recursiveParser = recursiveParser;

const takeRight = lParser => rParser => pipeParsers([lParser, rParser]); //           takeLeft :: Parser e a b -> Parser f b c -> Parser e a b


exports.takeRight = takeRight;

const takeLeft = lParser => rParser => sequenceOf([lParser, rParser]).map(x => x[0]); //           toPromise :: Either a b -> Promise a b


exports.takeLeft = takeLeft;

const toPromise = result => {
  return result.cata({
    Left: x => Promise.reject(x),
    Right: x => Promise.resolve(x)
  });
}; //           toValue :: Either a b -> b


exports.toPromise = toPromise;

const toValue = result => {
  return result.cata({
    Left: ([index, x]) => {
      const e = new Error(x);
      e.parseIndex = index;
      throw e;
    },
    Right: x => x
  });
};

exports.toValue = toValue;

},{"data.either":3}],2:[function(require,module,exports){
// Copyright (c) 2013-2014 Quildreen Motta <quildreen@gmail.com>
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation files
// (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

/**
 * @module lib/either
 */
module.exports = Either

// -- Aliases ----------------------------------------------------------
var clone         = Object.create
var unimplemented = function(){ throw new Error('Not implemented.') }
var noop          = function(){ return this                         }


// -- Implementation ---------------------------------------------------

/**
 * The `Either(a, b)` structure represents the logical disjunction between `a`
 * and `b`. In other words, `Either` may contain either a value of type `a` or
 * a value of type `b`, at any given time. This particular implementation is
 * biased on the right value (`b`), thus projections will take the right value
 * over the left one.
 *
 * This class models two different cases: `Left a` and `Right b`, and can hold
 * one of the cases at any given time. The projections are, none the less,
 * biased for the `Right` case, thus a common use case for this structure is to
 * hold the results of computations that may fail, when you want to store
 * additional information on the failure (instead of throwing an exception).
 *
 * Furthermore, the values of `Either(a, b)` can be combined and manipulated by
 * using the expressive monadic operations. This allows safely sequencing
 * operations that may fail, and safely composing values that you don't know
 * whether they're present or not, failing early (returning a `Left a`) if any
 * of the operations fail.
 *
 * While this class can certainly model input validations, the [Validation][]
 * structure lends itself better to that use case, since it can naturally
 * aggregate failures — monads shortcut on the first failure.
 *
 * [Validation]: https://github.com/folktale/data.validation
 *
 *
 * @class
 * @summary
 * Either[α, β] <: Applicative[β]
 *               , Functor[β]
 *               , Chain[β]
 *               , Show
 *               , Eq
 */
function Either() { }

Left.prototype = clone(Either.prototype)
function Left(a) {
  this.value = a
}

Right.prototype = clone(Either.prototype)
function Right(a) {
  this.value = a
}

// -- Constructors -----------------------------------------------------

/**
 * Constructs a new `Either[α, β]` structure holding a `Left` value. This
 * usually represents a failure due to the right-bias of this structure.
 *
 * @summary a → Either[α, β]
 */
Either.Left = function(a) {
  return new Left(a)
}
Either.prototype.Left = Either.Left

/**
 * Constructs a new `Either[α, β]` structure holding a `Right` value. This
 * usually represents a successful value due to the right bias of this
 * structure.
 *
 * @summary β → Either[α, β]
 */
Either.Right = function(a) {
  return new Right(a)
}
Either.prototype.Right = Either.Right


// -- Conversions ------------------------------------------------------

/**
 * Constructs a new `Either[α, β]` structure from a nullable type.
 *
 * Takes the `Left` case if the value is `null` or `undefined`. Takes the
 * `Right` case otherwise.
 *
 * @summary α → Either[α, α]
 */
Either.fromNullable = function(a) {
  return a != null?       new Right(a)
  :      /* otherwise */  new Left(a)
}
Either.prototype.fromNullable = Either.fromNullable

/**
 * Constructs a new `Either[α, β]` structure from a `Validation[α, β]` type.
 *
 * @summary Validation[α, β] → Either[α, β]
 */
Either.fromValidation = function(a) {
  return a.fold(Either.Left, Either.Right)
}

/**
 * Executes a synchronous computation that may throw and converts it to an
 * Either type.
 *
 * @summary (α₁, α₂, ..., αₙ -> β :: throws γ) -> (α₁, α₂, ..., αₙ -> Either[γ, β])
 */
Either.try = function(f) {
  return function() {
    try {
      return new Right(f.apply(null, arguments))
    } catch(e) {
      return new Left(e)
    }
  }
}


// -- Predicates -------------------------------------------------------

/**
 * True if the `Either[α, β]` contains a `Left` value.
 *
 * @summary Boolean
 */
Either.prototype.isLeft = false
Left.prototype.isLeft   = true

/**
 * True if the `Either[α, β]` contains a `Right` value.
 *
 * @summary Boolean
 */
Either.prototype.isRight = false
Right.prototype.isRight  = true


// -- Applicative ------------------------------------------------------

/**
 * Creates a new `Either[α, β]` instance holding the `Right` value `b`.
 *
 * `b` can be any value, including `null`, `undefined` or another
 * `Either[α, β]` structure.
 *
 * @summary β → Either[α, β]
 */
Either.of = function(a) {
  return new Right(a)
}
Either.prototype.of = Either.of


/**
 * Applies the function inside the `Right` case of the `Either[α, β]` structure
 * to another applicative type.
 *
 * The `Either[α, β]` should contain a function value, otherwise a `TypeError`
 * is thrown.
 *
 * @method
 * @summary (@Either[α, β → γ], f:Applicative[_]) => f[β] → f[γ]
 */
Either.prototype.ap = unimplemented

Left.prototype.ap = function(b) {
  return this
}

Right.prototype.ap = function(b) {
  return b.map(this.value)
}


// -- Functor ----------------------------------------------------------

/**
 * Transforms the `Right` value of the `Either[α, β]` structure using a regular
 * unary function.
 *
 * @method
 * @summary (@Either[α, β]) => (β → γ) → Either[α, γ]
 */
Either.prototype.map = unimplemented
Left.prototype.map   = noop

Right.prototype.map = function(f) {
  return this.of(f(this.value))
}


// -- Chain ------------------------------------------------------------

/**
 * Transforms the `Right` value of the `Either[α, β]` structure using an unary
 * function to monads.
 *
 * @method
 * @summary (@Either[α, β], m:Monad[_]) => (β → m[γ]) → m[γ]
 */
Either.prototype.chain = unimplemented
Left.prototype.chain   = noop

Right.prototype.chain = function(f) {
  return f(this.value)
}

// -- Semigroup ----------------------------------------------------------

/**
 * Concats the `Right` value of the `Either[α, β]` structure with another `Right` or keeps the `Left` on either side
 *
 * @method
 * @summary (@Either[α, m:Monoid]) => Either[β, m] → Either[α, m]
 */
Either.prototype.concat = unimplemented

Left.prototype.concat = function(other) {
  return this
}

Right.prototype.concat = function(other) {
  var that = this
  return other.fold(function(_){
                      return other
                    },
                    function(y) {
                      return that.Right(that.value.concat(y))
                    })
}


// -- Show -------------------------------------------------------------

/**
 * Returns a textual representation of the `Either[α, β]` structure.
 *
 * @method
 * @summary (@Either[α, β]) => Void → String
 */
Either.prototype.toString = unimplemented

Left.prototype.toString = function() {
  return 'Either.Left(' + this.value + ')'
}

Right.prototype.toString = function() {
  return 'Either.Right(' + this.value + ')'
}


// -- Eq ---------------------------------------------------------------

/**
 * Tests if an `Either[α, β]` structure is equal to another `Either[α, β]`
 * structure.
 *
 * @method
 * @summary (@Either[α, β]) => Either[α, β] → Boolean
 */
Either.prototype.isEqual = unimplemented

Left.prototype.isEqual = function(a) {
  return a.isLeft && (a.value === this.value)
}

Right.prototype.isEqual = function(a) {
  return a.isRight && (a.value === this.value)
}


// -- Extracting and recovering ----------------------------------------

/**
 * Extracts the `Right` value out of the `Either[α, β]` structure, if it
 * exists. Otherwise throws a `TypeError`.
 *
 * @method
 * @summary (@Either[α, β]) => Void → β         :: partial, throws
 * @see {@link module:lib/either~Either#getOrElse} — A getter that can handle failures.
 * @see {@link module:lib/either~Either#merge} — The convergence of both values.
 * @throws {TypeError} if the structure has no `Right` value.
 */
Either.prototype.get = unimplemented

Left.prototype.get = function() {
  throw new TypeError("Can't extract the value of a Left(a).")
}

Right.prototype.get = function() {
  return this.value
}


/**
 * Extracts the `Right` value out of the `Either[α, β]` structure. If the
 * structure doesn't have a `Right` value, returns the given default.
 *
 * @method
 * @summary (@Either[α, β]) => β → β
 */
Either.prototype.getOrElse = unimplemented

Left.prototype.getOrElse = function(a) {
  return a
}

Right.prototype.getOrElse = function(_) {
  return this.value
}


/**
 * Transforms a `Left` value into a new `Either[α, β]` structure. Does nothing
 * if the structure contain a `Right` value.
 *
 * @method
 * @summary (@Either[α, β]) => (α → Either[γ, β]) → Either[γ, β]
 */
Either.prototype.orElse = unimplemented
Right.prototype.orElse  = noop

Left.prototype.orElse = function(f) {
  return f(this.value)
}


/**
 * Returns the value of whichever side of the disjunction that is present.
 *
 * @summary (@Either[α, α]) => Void → α
 */
Either.prototype.merge = function() {
  return this.value
}


// -- Folds and Extended Transformations -------------------------------

/**
 * Applies a function to each case in this data structure.
 *
 * @method
 * @summary (@Either[α, β]) => (α → γ), (β → γ) → γ
 */
Either.prototype.fold = unimplemented

Left.prototype.fold = function(f, _) {
  return f(this.value)
}

Right.prototype.fold = function(_, g) {
  return g(this.value)
}

/**
 * Catamorphism.
 * 
 * @method
 * @summary (@Either[α, β]) => { Left: α → γ, Right: β → γ } → γ
 */
Either.prototype.cata = unimplemented

Left.prototype.cata = function(pattern) {
  return pattern.Left(this.value)
}

Right.prototype.cata = function(pattern) {
  return pattern.Right(this.value)
}


/**
 * Swaps the disjunction values.
 *
 * @method
 * @summary (@Either[α, β]) => Void → Either[β, α]
 */
Either.prototype.swap = unimplemented

Left.prototype.swap = function() {
  return this.Right(this.value)
}

Right.prototype.swap = function() {
  return this.Left(this.value)
}


/**
 * Maps both sides of the disjunction.
 *
 * @method
 * @summary (@Either[α, β]) => (α → γ), (β → δ) → Either[γ, δ]
 */
Either.prototype.bimap = unimplemented

Left.prototype.bimap = function(f, _) {
  return this.Left(f(this.value))
}

Right.prototype.bimap = function(_, g) {
  return this.Right(g(this.value))
}


/**
 * Maps the left side of the disjunction.
 *
 * @method
 * @summary (@Either[α, β]) => (α → γ) → Either[γ, β]
 */
Either.prototype.leftMap = unimplemented
Right.prototype.leftMap  = noop

Left.prototype.leftMap = function(f) {
  return this.Left(f(this.value))
}

},{}],3:[function(require,module,exports){
// Copyright (c) 2013-2014 Quildreen Motta <quildreen@gmail.com>
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation files
// (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

module.exports = require('./either')
},{"./either":2}],4:[function(require,module,exports){
const A = require('arcsecond');

let recognition = new webkitSpeechRecognition();
recognition.lang = 'en-US';
recognition.interimResults = false;
recognition.maxAlternatives = 1;

recognition.onnomatch = function() {
  console.log("I didn't recognize that command.");
}

recognition.onerror = function(event) {
  console.log(event.error);
}

recognition.onresult = function(event) {
  var command = event.results[0][0].transcript;
  do_command(command);
  /*
  console.log(command);
  switch (command) {
    case "case":
    case "construct case":
      let construct_case = new Event("construct_case");
      document.dispatchEvent(construct_case);
      break;
    case "lambda":
    case "construct lambda":
    case "function":
    case "construct function":
      let construct_lam = new Event("construct_lam");
      document.dispatchEvent(construct_lam);
      break;
    case "let":
    case "construct let":
      let construct_let = new Event("construct_let");
      document.dispatchEvent(construct_let);
      break;
    case "wild":
    case "construct wild":
    case "underscore":
    case "construct underscore":
      let construct_wild = new Event("construct_wild");
      document.dispatchEvent(construct_wild);
      break;
    case "colon":
    case "construct colon":
      document.dispatchEvent(construct_asc);
      break;
    default:
      break;
  }
  */
}

let space = A.many1(A.str(" "));

let variable = //A.pipeParsers([
  A.sequenceOf([
    A.str("variable"),
    space,
    A.many(A.anythingExcept(A.whitespace))
  ]);
  /*
  A.mapTo(([variable, space, name]) => {
    [new CustomEvent("construct_var", { name })]
  })
  */
//  A.mapTo(strings => strings.join(" "))
//]);

let parser = A.choice([
  variable
]);

/*
let let_complex = A.pipeParsers([
  A.sequenceOf([
    let_simple,
    space,
    variable,
    space,
    equal,
    space,
    expression
  ]),
  mapTo(([lett, space, equal, expression]))
]);
*/

/*
function do_command(command) {
  let events = A.parse(parser).value;
  console.log(events);
}
*/

let backspace = new Event("backspace");
let del = new Event("delete");
let construct_case = new Event("construct_case");
let construct_lam = new Event("construct_lam");
let construct_let = new Event("construct_let");
let construct_asc = new Event("construct_asc");

let num_words = {
  "one": 1,
  "two": 2,
  "three": 3,
  "four": 4,
  "five": 5,
  "six": 6,
  "seven": 7,
  "eight": 8,
  "nine": 9,
  "ten": 10
};

function do_command(command) {
  let tokens = command.toLowerCase().split(" ");
  console.log(tokens);
  switch (tokens.length) {
    case 0:
      break;
    case 1:
      switch (tokens[0]) {
        case "backspace":
          document.dispatchEvent(backspace);
          break;
        case "delete":
          document.dispatchEvent(del);
          break;
        case "case":
          document.dispatchEvent(construct_case);
          break;
        case "function":
        case "lambda":
          document.dispatchEvent(construct_lam);
          break;
        case "define":
          document.dispatchEvent(construct_let);
          break;
        case "ascription":
          document.dispatchEvent(construct_asc);
          break;
        case "empty":
          document.dispatchEvent(new Event("construct_nil"));
          break;
        case "plus":
          document.dispatchEvent(new Event("construct_plus"));
          break;
        case "multiply":
          document.dispatchEvent(new Event("construct_times"));
          break;
        case "space":
          document.dispatchEvent(new Event("construct_space"));
          break;
        case "head":
          document.dispatchEvent(new Event("construct_cons"));
          break;
        case "line":
          document.dispatchEvent(new Event("construct_line"));
          break;
        case "parentheses":
          document.dispatchEvent(new Event("construct_paren"));
          break;
        default:
          break;
      }
      break;
    case 2:
      switch (tokens[0]) {
        case "variable":
          let construct_var = new CustomEvent("construct_var", { detail: tokens[1] });
          document.dispatchEvent(construct_var);
          break;
        case "number":
          let digit = num_words[tokens[1]];
          console.log("digit");
          console.log(digit);
          var detail;
          if (digit) {
            detail = digit;
          } else {
            detail = tokens[1];
          }
          let construct_num = new CustomEvent("construct_num", { detail: detail })
          document.dispatchEvent(construct_num);
          break;
        default:
          break;
      }
    default:
      break;
  }
}

var interval;
var held = false;

$(document).on('keydown', function(e) {
  if (e.key == " ") {
    if (interval == null) {
      held = false;
      interval = setInterval(function() {
        if (!held) {
          startRecording();
          held = true;
        }
      }, 100);
    }
  }
}).on('keyup', function(e) {
  if (e.key == " ") {
    clearInterval(interval); 
    interval = null;
    if (held) {
      stopRecording();
    }
  }
});

function startRecording() {
  recognition.start();
}

function stopRecording() {
  recognition.stop();
}

},{"arcsecond":1}]},{},[4]);
