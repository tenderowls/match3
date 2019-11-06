(function() {
  // https://github.com/Modernizr/Modernizr/blob/master/feature-detects/touchevents.js
  function is_touch_device() {
    var prefixes = ' -webkit- -moz- -o- -ms- '.split(' ');
    var mq = function(query) {
      return window.matchMedia(query).matches;
    }
    if (('ontouchstart' in window) || window.DocumentTouch && document instanceof DocumentTouch) {
      return true;
    }
    var query = ['(', prefixes.join('touch-enabled),('), 'heartz', ')'].join('');
    return mq(query);
  }
  const IS_TOUCH_DEVICE = is_touch_device();
  const MOUSE_MOVE = IS_TOUCH_DEVICE ? 'touchmove' : 'mousemove';
  const MOUSE_DOWN = IS_TOUCH_DEVICE ? 'touchstart' : 'mousedown';
  const MOUSE_UP = IS_TOUCH_DEVICE ? 'touchend' : 'mouseup';
  const THRESHOLD = 20;
  window.onload = () => {
    let onMouseDown = (mouseDown) => {
      let startX = mouseDown.clientX || mouseDown.touches[0].clientX;
      let startY = mouseDown.clientY || mouseDown.touches[0].clientY;
      var shouldPreventMouseUp = false;
      let onMouseMove = (mouseMove) => {
        let finishX = mouseMove.clientX || mouseMove.changedTouches[0].clientX;
        let finishY = mouseMove.clientY || mouseMove.changedTouches[0].clientY;
        let dx = startX - finishX;
        let dy = startY - finishY;
        // Recognize direction
        let up = dy != 0 && dy >= THRESHOLD;
        let down = dy != 0 && dy <= -THRESHOLD;
        let left = dx != 0 && dx >= THRESHOLD;
        let right = dx != 0 && dx <= -THRESHOLD;
        if (up || down || left || right) {
          shouldPreventMouseUp = true;
        }
//        console.log(`dx = ${dx}, dy = ${dy}`)
//        console.log(`up = ${up}, down = ${down}, left = ${left}, right = ${right}`)
        function dispatchEvent(name) {
          let event = new CustomEvent(name, {
            'bubbles': true
          });
          document.body.removeEventListener(MOUSE_MOVE, onMouseMove);
          mouseDown.target.dispatchEvent(event);
        }
        if (up) dispatchEvent('swipeup');
        if (down) dispatchEvent('swipedown');
        if (left) dispatchEvent('swipeleft');
        if (right) dispatchEvent('swiperight');
      }
      let onMouseUp = (mouseUp) => {
        if (shouldPreventMouseUp)
          mouseUp.preventDefault();
        document.body.removeEventListener(MOUSE_MOVE, onMouseMove);
      }
      document.body.addEventListener(MOUSE_UP, onMouseUp, { 'once': true });
      document.body.addEventListener(MOUSE_MOVE, onMouseMove);
    }
    document.body.addEventListener(MOUSE_DOWN, onMouseDown);
  };
}).call(this);