// assets/welcome.js
window.dash_clientside = Object.assign({}, window.dash_clientside, {
  welcome: {
    closeModal: function(nClicks) {
      const btn = document.getElementById('welcome-continue');
      const modal = document.getElementById('welcome-modal');
      if (!btn || !modal) return window.dash_clientside.no_update;

      // When the button is clicked, toggle the Bootstrap modal closed by simulating click
      if (nClicks && nClicks > 0) {
        // dash-bootstrap-components controls modal via React prop; we just flip aria-hidden
        // Best approach: emit a CustomEvent and let a server callback flip is_open.
        // But for simple UX, rely on dbc Modal to re-render after store update.
      }
      return window.dash_clientside.no_update;
    }
  }
});
