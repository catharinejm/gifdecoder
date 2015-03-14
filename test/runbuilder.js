#!/usr/bin/env node

fs = require('fs');
if (process.argv.length < 3) {
  console.error("Requires filename!");
  process.exit(1);
}

var lines = [];
var regexp = /^\s*DEFTEST\s*\(\s*(\w+)\s*(?:,[\s\w]*)*\).*$/;

for (var i = 2; i < process.argv.length; i++) {
  var file = process.argv[i];
  var data = fs.readFileSync(file, 'utf8');

  var matches = data.split(/\r?\n/).filter(function(l) { return l.match(regexp); });
  for (var j in matches) {
    lines.push(matches[j]);
  }
}

for (var i in lines) {
  lines[i] = lines[i].replace(regexp, '$1();');
}
var externs = [];
for (var i in lines) {
  externs.push("void "+lines[i]);
}
console.log("-DRUN_TESTS\\(\\)='do { "+lines.join("")+" } while (0)' "+
            "-DTEST_EXTERNS='"+externs.join("")+"' "+
            "-DTEST_COUNT="+lines.length);
