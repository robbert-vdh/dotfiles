// ES5 15.9.4.4
// http://es5.github.com/#x15.9.4.4
if (!Date.now) {
   Date.now = function now() {
       return new Date().getTime();
   };
}