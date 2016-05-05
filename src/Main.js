'use strict';

// module Main

exports['main\''] = function(term, alphaConvert, betaReduce, etaConvert, render) {
  return function() {
    addEventListener('load', function() {
      var termView = document.getElementById('term');

      var transformer = function(transform) {
        return function() {
          term = transform(term);
          termView.textContent = render(term);
        };
      };

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
