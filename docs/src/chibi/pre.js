Module['preRun'].push(function () {
  FS.writeFile('program.scm', Module['program']);
  FS.writeFile('target.txt', Module['program']);
});
Module['arguments'] = Module['arguments'] || [];
Module['arguments'].unshift('program.scm', 'target.txt');

