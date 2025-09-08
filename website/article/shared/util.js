function toggleDataPane(gridId) {
   const grid = document.getElementById(gridId)
   const hidden = grid.classList.contains('data-pane-hidden')
   const dataPaneButton = document.querySelector('.data-pane-button')

   if (hidden) {
      grid.classList.remove('data-pane-hidden')
      dataPaneButton.classList.remove('fa-eye-slash')
      dataPaneButton.classList.add('fa-eye')
   } else {
      grid.classList.add('data-pane-hidden')
      dataPaneButton.classList.remove('fa-eye')
      dataPaneButton.classList.add('fa-eye-slash')
   }
}
