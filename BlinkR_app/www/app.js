document.addEventListener('DOMContentLoaded', function () {
  const buttons = document.querySelectorAll('.custom-action');
  buttons.forEach(button => {
    button.addEventListener('click', function () {
      // Add the Unicode checkmark to the button's text
      if (!button.innerHTML.includes('✔')) {
        button.innerHTML += ' &#x2705;';
      }
    });
  });
});
