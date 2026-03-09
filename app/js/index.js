// app/js/index.js

$(document).on('shiny:connected', () => {
  // eslint-disable-next-line no-unused-vars
  Shiny.addCustomMessageHandler('toggle-btn', (_msg) => {
    const btn = document.getElementById(_msg.id);
    if (btn) {
      btn.disabled = _msg.disabled;
      btn.classList.toggle('disabled', _msg.disabled);
    }
  });
});
