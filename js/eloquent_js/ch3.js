exports.mymin = function(a,b) {
  return (a<b) ? a : b;
};

function isEven(number) {
  if (number == 0) {return true;}
  else if (number == 1) {return false;}
  else {return isEven(number-2);}
}

exports.isEven = isEven

function countBs(s) {
  return countChar(s,'B');
}

function countChar(s, c) {
  return s.split('').filter(v => v == c).length;
}

exports.countBs = countBs;
exports.countChar = countChar;
