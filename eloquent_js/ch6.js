function Vector(x,y) {
  this.x = x;
  this.y = y;
}

Vector.prototype.plus = function(vec) {
  return new Vector(this.x + vec.x, this.y + vec.y);
}

Vector.prototype.minus = function(vec) {
  return new Vector(this.x - vec.x, this.y - vec.y);
}

Object.defineProperty(Vector.prototype, 'length', {
        get: function() { return Math.sqrt(Math.pow(this.x,2) + Math.pow(this.y,2)); }       
});

exports.Vector = Vector;

function rowHeights(rows) {
  return rows.map(function(row) {
    return row.reduce(function(max, cell) {
      return Math.max(max, cell.minHeight());
    }, 0);
  });
}

function colWidths(rows) {
  return rows[0].map(function(_, i) {
    return rows.reduce(function(max, row) {
      return Math.max(max, row[i].minWidth());
    }, 0);
  });
}

function drawTable(rows) {
  var heights = rowHeights(rows);
  var widths = colWidths(rows);

  function drawLine(blocks, lineNo) {
    return blocks.map(function(block) {
      return block[lineNo];
    }).join(" ");
  }
  function drawRow(row, rowNum) {
    var blocks = row.map(function(cell, colNum) {
      return cell.draw(widths[colNum], heights[rowNum]);
    });
    return blocks[0].map(function(_, lineNo) {
      return drawLine(blocks, lineNo);
    }).join("\n");
  }

  return rows.map(drawRow).join("\n");
}

function repeat(string, times) {
  var result = "";
  for (var i = 0; i < times; i++)
    result += string;
  return result;
}

function dataTable(data) {
  var keys = Object.keys(data[0]);
  var headers = keys.map(function(name) {
    return new UnderlinedCell(new TextCell(name));
  });
  var body = data.map(function(row) {
    return keys.map(function(name) {
      return new TextCell(String(row[name]));
    });
  });
  return [headers].concat(body);
}


// TextCell
function TextCell(text) {
  this.text = text.split("\n");
}

TextCell.prototype.minWidth = function() {
  return this.text.reduce(function(width, line) {
    return Math.max(width, line.length);
}, 0);
};

TextCell.prototype.minHeight = function() {
  return this.text.length;
};

TextCell.prototype.draw = function(width, height) {
  var result = [];
  for (var i = 0; i < height; i++) {
    var line = this.text[i] || "";
    result.push(line + repeat(" ", width - line.length));
  }
  return result;
};

// UnderlinedCell
function UnderlinedCell(inner) {
  this.inner = inner;
};
UnderlinedCell.prototype.minWidth = function() {
  return this.inner.minWidth();
};
UnderlinedCell.prototype.minHeight = function() {
  return this.inner.minHeight() + 1;
};
UnderlinedCell.prototype.draw = function(width, height) {
  return this.inner.draw(width, height - 1)
    .concat([repeat("-", width)]);
};

// StretchCell
function StretchCell(inner, width, height) {
  this.inner = inner;
  this.width = width;
  this.height = height;
};
StretchCell.prototype.minWidth = function() { return Math.max(this.inner.minWidth(), this.width); }
StretchCell.prototype.minHeight = function() { return Math.max(this.inner.minHeight(), this.height); }
StretchCell.prototype.draw = function(width, height) { return this.inner.draw(width, height); }
var rows = [];
for (var i = 0; i < 5; i++) {
   var row = [];
   for (var j = 0; j < 5; j++) {
     if ((j + i) % 2 == 0)
       row.push(new TextCell("##"));
     else
       row.push(new TextCell(" "));
   }
   rows.push(row);
}

var MOUNTAINS = require('./mountains.js');
console.log(drawTable(rows));

exports.drawTable = drawTable;
exports.foo = drawTable(dataTable(MOUNTAINS));
exports.StretchCell = StretchCell;
exports.TextCell = TextCell;

// Sequence Interface

function ISeq(seq) {
  this.seq = seq;
  this.it = 0;
};
ISeq.prototype.hasNext() {
  return this.it < this.seq-1;
}
ISeq.prototype.next() {
  const o = this.seq[this.it];
  this.it += 1;
  return o;
}


