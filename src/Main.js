'use strict';

// module Main

exports.varNode = function(free) {
  return function(name) {
    var e = document.createElement('span');
    e.classList.add('var');
    if (free) {
      e.classList.add('free');
    }
    e.textContent = name;
    return e;
  };
};

exports.absNode = function(p) {
  return function(b) {
    var e = document.createElement('span');
    e.classList.add('abs');
    e.appendChild(document.createTextNode('λ'));
    e.appendChild(document.createTextNode(p));
    e.appendChild(document.createTextNode('.'));
    e.appendChild(b);
    return e;
  };
};

exports.appNode = function(c) {
  return function(a) {
    var e = document.createElement('span');
    e.classList.add('app');

    if (c.classList.contains('abs')) {
      e.appendChild(document.createTextNode('('));
    }
    e.appendChild(c);
    if (c.classList.contains('abs')) {
      e.appendChild(document.createTextNode(')'));
    }

    e.appendChild(document.createTextNode(' '));

    if (a.classList.contains('app')) {
      e.appendChild(document.createTextNode('('));
    }
    e.appendChild(a);
    if (a.classList.contains('app')) {
      e.appendChild(document.createTextNode(')'));
    }
    return e;
  };
};

exports['main\''] = function(term, parse, alphaConvert, betaReduce, etaConvert, render) {
  return function() {
    addEventListener('load', function() {
      var termView = document.getElementById('term');

      var transformer = function(transform) {
        return function() {
          term = transform(term);
          if (termView.firstChild) {
            termView.removeChild(termView.firstChild);
          }
          termView.appendChild(render(term));
        };
      };

      document.getElementById('edit').addEventListener('click', function() {
        var text = prompt();
        transformer(function() { return parse(text); })();
      });

      document.getElementById('alpha-convert')
        .addEventListener('click', transformer(alphaConvert));
      document.getElementById('beta-reduce')
        .addEventListener('click', transformer(betaReduce));
      document.getElementById('eta-convert')
        .addEventListener('click', transformer(etaConvert));

      transformer(function(x) { return x; })();
    });
  };
};
