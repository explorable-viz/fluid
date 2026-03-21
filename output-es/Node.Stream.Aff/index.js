// | Asynchronous I/O with the [*Node.js* Stream API](https://nodejs.org/docs/latest/api/stream.html).
// |
// | Open __file streams__ with
// | [__Node.FS.Stream__](https://pursuit.purescript.org/packages/purescript-node-fs/docs/Node.FS.Stream).
// |
// | Open __process streams__ with
// | [__Node.Process__](https://pursuit.purescript.org/packages/purescript-node-process/docs/Node.Process).
// |
// | All __I/O errors__ will be thrown through the `Aff` `MonadError` class
// | instance.
// |
// | `Aff` __cancellation__ will clean up all *Node.js* event listeners.
// |
// | All of these `Aff` functions will prevent the *Node.js* __event loop__ from
// | exiting until the `Aff` function completes.
// |
// | ## Reading
// |
// | #### Implementation
// |
// | The `read*` functions (not to be confused with the `readable*` functions)
// | in this module all operate on a `Readable` stream
// | in
// | [“paused mode”](https://nodejs.org/docs/latest/api/stream.html#stream_two_reading_modes).
// |
// | Internally the reading functions use the
// | [`readable.read([size])`](https://nodejs.org/docs/latest/api/stream.html#readablereadsize)
// | function and are subject to the caveats of that function.
// |
// | #### Result Buffers
// |
// | The result of a reading function may be chunked into more than one `Buffer`.
// | The `buffers` element of the result is an `Array Buffer` of what
// | was read.
// | To concatenate the result into a single `Buffer`, use
// | [`Node.Buffer.concat :: Array Buffer -> m Buffer`](https://pursuit.purescript.org/packages/purescript-node-buffer/docs/Node.Buffer#t:MutableBuffer).
// |
// | ```
// | input :: Buffer
// |     <- liftEffect <<< concat <<< _.buffers =<< readSome stdin
// | ```
// |
// | To calculate the number of bytes read, use
// | `Node.Buffer.size :: Buffer -> m Int`.
// |
// | ```
// | { buffers } :: Array Buffer <- readSome stdin
// | bytesRead :: Int
// |     <- liftEffect $ Array.foldM (\a b -> (a+_) <$> size b) 0 buffers
// | ```
// |
// | #### Result `readagain` flag
// |
// | The `readagain` field of the result is a `Boolean` flag which
// | is `true` if the stream has not reached End-Of-File (and also if the stream
// | has not errored or been destroyed), so we know we can read again.
// | If the flag is `false` then the stream is not `readable`;
// | no more bytes will ever be produced by the stream.
// |
// | Reading from an ended, closed, errored, or destroyed stream
// | will complete immediately with `{ buffers: [], readagain: false }`.
// |
// | The `readagain` flag will give the same answer as a
// | subsequent call to `readable`.
// |
// | ## Writing
// |
// | #### Implementation
// |
// | The writing functions in this module all operate on a `Writeable` stream.
// |
// | Internally the writing functions will call the
// | [`writable.write(chunk[, encoding][, callback])`](https://nodejs.org/docs/latest/api/stream.html#writablewritechunk-encoding-callback)
// | function on each of the `Buffer`s,
// | and will asychronously wait if there is “backpressure” from the stream.
// |
// | #### Result
// |
// | The writing functions will complete after all the data is flushed to the
// | stream.
// |
// | If a write fails then it will `throwError` in the `Aff`.
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dRec$dClass from "../Control.Monad.Rec.Class/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import * as Node$dBuffer from "../Node.Buffer/index.js";
import * as Node$dBuffer$dImmutable from "../Node.Buffer.Immutable/index.js";
import * as Node$dEncoding from "../Node.Encoding/index.js";
import * as Node$dEventEmitter from "../Node.EventEmitter/index.js";
import * as Node$dStream from "../Node.Stream/index.js";
const write = dictMonadAff => w => bs => dictMonadAff.liftAff(Effect$dAff.makeAff(complete => () => {
  let removeDrain = () => {};
  const oneWrite = i$p => Control$dMonad$dRec$dClass.monadRecEffect.tailRecM(i => {
    if (i >= 0 && i < bs.length) {
      const $0 = bs[i];
      return () => {
        const nobackpressure = Node$dStream.writeCbImpl(
          w,
          $0,
          err => {
            const $1 = Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just);
            if ($1.tag === "Nothing") { return; }
            if ($1.tag === "Just") { return complete(Data$dEither.$Either("Left", $1._1))(); }
            $runtime.fail();
          }
        );
        if (nobackpressure) { return Control$dMonad$dRec$dClass.$Step("Loop", i + 1 | 0); }
        const removeDrain$p = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("drain", Node$dStream.identity))(oneWrite(i + 1 | 0))(w)();
        removeDrain = removeDrain$p;
        return Control$dMonad$dRec$dClass.$Step("Done", undefined);
      };
    }
    const $0 = complete(Data$dEither.$Either("Right", undefined));
    return () => {
      $0();
      return Control$dMonad$dRec$dClass.$Step("Done", undefined);
    };
  })(i$p);
  oneWrite(0)();
  const $0 = Effect$dAff._liftEffect(() => {
    const $0 = removeDrain;
    return $0();
  });
  return v => $0;
}));
const toStringUTF8 = dictMonadEffect => bs => dictMonadEffect.liftEffect(() => {
  const $0 = Node$dBuffer.concat(bs)();
  return Node$dBuffer$dImmutable.toString(Node$dEncoding.UTF8)($0);
});
const readableToBuffers = dictMonadAff => r => dictMonadAff.liftAff(Effect$dAff.makeAff(complete => () => {
  const bufs = [];
  let dataRef = () => {};
  const removeData = () => {
    const $0 = dataRef;
    return $0();
  };
  const removeError = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("error", Effect$dUncurried.mkEffectFn1))(err => () => {
    removeData();
    return complete(Data$dEither.$Either("Left", err))();
  })(r)();
  const removeClose = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("close", Node$dStream.identity))(() => {
    removeError();
    removeData();
    return complete(Data$dEither.$Either("Right", bufs))();
  })(r)();
  const removeEnd = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("end", Node$dStream.identity))(() => {
    removeError();
    removeClose();
    removeData();
    return complete(Data$dEither.$Either("Right", bufs))();
  })(r)();
  const rmData = Node$dEventEmitter.on(Node$dStream.dataH)(buf => () => {bufs.push(buf);})(r)();
  dataRef = rmData;
  const $0 = Effect$dAff._liftEffect(() => {
    removeError();
    removeClose();
    removeEnd();
    return removeData();
  });
  return v => $0;
}));
const readableToString = dictMonadAff => {
  const MonadEffect0 = dictMonadAff.MonadEffect0();
  return r => enc => MonadEffect0.Monad0().Bind1().bind(readableToBuffers(dictMonadAff)(r))(bufs => MonadEffect0.liftEffect(() => {
    const $0 = Node$dBuffer.concat(bufs)();
    return Node$dBuffer$dImmutable.toString(enc)($0);
  }));
};
const readableToStringUtf8 = dictMonadAff => {
  const readableToString1 = readableToString(dictMonadAff);
  return r => readableToString1(r)(Node$dEncoding.UTF8);
};
const readSome = dictMonadAff => r => dictMonadAff.liftAff(Effect$dAff.makeAff(complete => () => {
  const isReadable = Node$dStream.readableImpl(r);
  if (isReadable) {
    const bufs = [];
    const removeError = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("error", Effect$dUncurried.mkEffectFn1))(err => complete(Data$dEither.$Either("Left", err)))(r)();
    const removeClose = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("close", Node$dStream.identity))(() => {
      removeError();
      return complete(Data$dEither.$Either("Right", {buffers: bufs, readagain: false}))();
    })(r)();
    const removeEnd = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("end", Node$dStream.identity))(() => {
      removeError();
      removeClose();
      return complete(Data$dEither.$Either("Right", {buffers: bufs, readagain: false}))();
    })(r)();
    return Effect$dException.catchException(err => () => {
      removeError();
      removeClose();
      removeEnd();
      complete(Data$dEither.$Either("Left", err))();
      return Effect$dAff.nonCanceler;
    })((() => {
      const $0 = Effect.untilE((() => {
        const $0 = Node$dStream.read(r);
        return () => {
          const v = $0();
          if (v.tag === "Nothing") { return true; }
          if (v.tag === "Just") {
            bufs.push(v._1);
            return false;
          }
          $runtime.fail();
        };
      })());
      return () => {
        $0();
        const readagain = Node$dStream.readableImpl(r);
        if (readagain && bufs.length === 0) {
          const removeReadable = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("readable", Node$dStream.identity))((() => {
            const $1 = Effect.untilE((() => {
              const $1 = Node$dStream.read(r);
              return () => {
                const v = $1();
                if (v.tag === "Nothing") { return true; }
                if (v.tag === "Just") {
                  bufs.push(v._1);
                  return false;
                }
                $runtime.fail();
              };
            })());
            return () => {
              $1();
              removeError();
              removeClose();
              removeEnd();
              const readagain2 = Node$dStream.readableImpl(r);
              return complete(Data$dEither.$Either("Right", {buffers: bufs, readagain: readagain2}))();
            };
          })())(r)();
          const $1 = Effect$dAff._liftEffect(() => {
            removeError();
            removeClose();
            removeEnd();
            return removeReadable();
          });
          return v => $1;
        }
        removeError();
        removeClose();
        removeEnd();
        complete(Data$dEither.$Either("Right", {buffers: bufs, readagain}))();
        return Effect$dAff.nonCanceler;
      };
    })())();
  }
  complete(Data$dEither.$Either("Right", {buffers: [], readagain: false}))();
  return Effect$dAff.nonCanceler;
}));
const readN = dictMonadAff => r => n => dictMonadAff.liftAff(Effect$dAff.makeAff(complete => {
  if (n < 0) {
    const $0 = complete(Data$dEither.$Either("Left", Effect$dException.error("read bytes must be > 0")));
    return () => {
      $0();
      return Effect$dAff.nonCanceler;
    };
  }
  return () => {
    const isReadable = Node$dStream.readableImpl(r);
    if (isReadable) {
      let redRef = 0;
      const bufs = [];
      let removeReadable = () => {};
      const removeError = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("error", Effect$dUncurried.mkEffectFn1))(err => () => {
        const $0 = removeReadable;
        $0();
        return complete(Data$dEither.$Either("Left", err))();
      })(r)();
      const removeClose = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("close", Node$dStream.identity))(() => {
        removeError();
        const $0 = removeReadable;
        $0();
        return complete(Data$dEither.$Either("Right", {buffers: bufs, readagain: false}))();
      })(r)();
      const removeEnd = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("end", Node$dStream.identity))(() => {
        removeError();
        removeClose();
        return complete(Data$dEither.$Either("Right", {buffers: bufs, readagain: false}))();
      })(r)();
      const tryToRead = continuation => {
        const $0 = Effect.untilE(() => {
          const red = redRef;
          const v = Node$dStream.read$p(r)(n - red | 0)();
          if (v.tag === "Nothing") { return true; }
          if (v.tag === "Just") {
            bufs.push(v._1);
            const $0 = redRef;
            const red$p = redRef = $0 + Node$dBuffer$dImmutable.size(v._1) | 0;
            if (red$p >= n) { return true; }
            return false;
          }
          $runtime.fail();
        });
        return () => {
          $0();
          const red = redRef;
          if (red >= n) {
            removeError();
            removeClose();
            removeEnd();
            const readagain = Node$dStream.readableImpl(r);
            return complete(Data$dEither.$Either("Right", {buffers: bufs, readagain}))();
          }
          return continuation()();
        };
      };
      const waitToRead = v => {
        const $0 = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("readable", Node$dStream.identity))(tryToRead(waitToRead))(r);
        return () => {
          const removeReadable$p = $0();
          return removeReadable = removeReadable$p;
        };
      };
      return Effect$dException.catchException(err => () => {
        removeError();
        removeClose();
        removeEnd();
        const $0 = removeReadable;
        $0();
        complete(Data$dEither.$Either("Left", err))();
        return Effect$dAff.nonCanceler;
      })((() => {
        const $0 = tryToRead(waitToRead);
        return () => {
          $0();
          const $1 = Effect$dAff._liftEffect(() => {
            removeError();
            removeClose();
            removeEnd();
            const $1 = removeReadable;
            return $1();
          });
          return v => $1;
        };
      })())();
    }
    complete(Data$dEither.$Either("Right", {buffers: [], readagain: false}))();
    return Effect$dAff.nonCanceler;
  };
}));
const readAll = dictMonadAff => r => dictMonadAff.liftAff(Effect$dAff.makeAff(complete => () => {
  const isReadable = Node$dStream.readableImpl(r);
  if (isReadable) {
    const bufs = [];
    let removeReadable = () => {};
    const removeError = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("error", Effect$dUncurried.mkEffectFn1))(err => () => {
      const $0 = removeReadable;
      $0();
      return complete(Data$dEither.$Either("Left", err))();
    })(r)();
    const removeClose = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("close", Node$dStream.identity))(() => {
      removeError();
      const $0 = removeReadable;
      $0();
      return complete(Data$dEither.$Either("Right", bufs))();
    })(r)();
    const removeEnd = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("end", Node$dStream.identity))(() => {
      removeError();
      removeClose();
      return complete(Data$dEither.$Either("Right", bufs))();
    })(r)();
    return Effect$dException.catchException(err => () => {
      removeError();
      removeClose();
      removeEnd();
      const $0 = removeReadable;
      $0();
      complete(Data$dEither.$Either("Left", err))();
      return Effect$dAff.nonCanceler;
    })((() => {
      const $0 = Effect.untilE((() => {
        const $0 = Node$dStream.read(r);
        return () => {
          const v = $0();
          if (v.tag === "Nothing") { return true; }
          if (v.tag === "Just") {
            bufs.push(v._1);
            return false;
          }
          $runtime.fail();
        };
      })());
      return () => {
        $0();
        const waitToRead$lazy = $runtime.binding(() => {
          const $1 = Node$dEventEmitter.once(Node$dEventEmitter.$EventHandle("readable", Node$dStream.identity))((() => {
            const $1 = Effect.untilE((() => {
              const $1 = Node$dStream.read(r);
              return () => {
                const v = $1();
                if (v.tag === "Nothing") { return true; }
                if (v.tag === "Just") {
                  bufs.push(v._1);
                  return false;
                }
                $runtime.fail();
              };
            })());
            return () => {
              $1();
              return waitToRead$lazy()();
            };
          })())(r);
          return () => {
            const removeReadable$p = $1();
            return removeReadable = removeReadable$p;
          };
        });
        const waitToRead = waitToRead$lazy();
        waitToRead();
        const $1 = Effect$dAff._liftEffect(() => {
          removeError();
          removeClose();
          removeEnd();
          const $1 = removeReadable;
          return $1();
        });
        return v => $1;
      };
    })())();
  }
  complete(Data$dEither.$Either("Right", []))();
  return Effect$dAff.nonCanceler;
}));
const fromStringUTF8 = dictMonadEffect => s => dictMonadEffect.liftEffect(() => [Node$dBuffer$dImmutable.fromString(s)(Node$dEncoding.UTF8)]);
const end = dictMonadAff => w => dictMonadAff.liftAff(Effect$dAff.makeAff(complete => () => {
  Node$dStream.endCbImpl(
    w,
    err => {
      const $0 = Data$dNullable.nullable(err, Data$dMaybe.Nothing, Data$dMaybe.Just);
      if ($0.tag === "Nothing") { return complete(Data$dEither.$Either("Right", undefined))(); }
      if ($0.tag === "Just") { return complete(Data$dEither.$Either("Left", $0._1))(); }
      $runtime.fail();
    }
  );
  return Effect$dAff.nonCanceler;
}));
export {end, fromStringUTF8, readAll, readN, readSome, readableToBuffers, readableToString, readableToStringUtf8, toStringUTF8, write};
