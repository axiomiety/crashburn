exports.tri_loop = function() {
  const disp = '#';
  for(var i=1; i <= 7; i+=1) {
    console.log(disp.repeat(i));
  }
};

exports.fizzbuzz = function() {
  for(var i=1;i<=100; i+=1) {
    var s = '';
    if (i%3 == 0) {s = 'fizz';};
    if (i%5 == 0) {s += 'buzz';};
    console.log(s || i);
  }
}

exports.chessboard = function(size=8) {
  for (var row=1; row<=size; row+=1) {
    var s = '';
    // this is ugly
    for (var col=1; col<=size; col+=1) {
      if (row%2) {s += col%2 ? '#' : ' ';}
      else {s += col%2 ? ' ' : '#';}
    }
    console.log(s);
  }
}
