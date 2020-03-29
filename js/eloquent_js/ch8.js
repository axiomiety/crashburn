function MultiplicatorUnitFailure() {}
MultiplicatorUnitFailure.prototype = Object.create(Error.prototype);

function primitiveMultiply(a,b) {
  if (Math.random() > 0.5) {throw new MultiplicatorUnitFailure("wah!");}
  return a*b;
}

function mul(a,b) {
        for (;;) {
        try {
          return primitiveMultiply(a,b);
        } catch(e) {
          if (!(e instanceof MultiplicatorUnitFailure)) {
            throw e;
          }
          console.log("MultiplicatorUnitFailure");
        }
        }
}
var box = {
  locked: true,
  unlock: function() {this.locked=false;},
  lock: function() {this.locked=true;},
  _contents: [],
  get content() {
          if (this.locked) throw new Error("Locked!");
          return this._content;
  }
}

function withBoxUnlocked(b) {
  try {
    return box.contents();
  } catch(e) {
  } finally {
    box.lock()
  }
}

exports.box = box;
exports.wbu = withBoxUnlocked;
exports.mul = mul;
