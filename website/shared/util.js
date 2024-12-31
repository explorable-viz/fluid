function loadHeader () {
   fetch('/shared/header.html')
      .then(response => response.text())
      .then(data => {
         const header = document.createElement('div')
         header.innerHTML = data
         activateCurrentLink(header)
         const divElement = header.children[0]
         const grid = document.getElementById('grid')
         grid.parentNode.insertBefore(divElement, grid)
      })
      .catch(error => console.error('Error loading shared HTML:', error))

   fetch('/shared/footer.html')
      .then(response => response.text())
      .then(data => {
         const footer = document.createElement('div')
         footer.innerHTML = data
         const divElement = footer.children[0]
         const grid = document.getElementById('grid')
         grid.parentNode.appendChild(divElement)
      })
      .catch(error => console.error('Error loading shared HTML:', error))
}

function eqPaths (path1, path2) {
   const trailingSlashes = /\/+$/
   return path1.replace(trailingSlashes, '') === path2.replace(trailingSlashes, '')
}

function activateCurrentLink (el) {
   const listItems = el.querySelectorAll('li')
   const n_ = Array.from(listItems).findIndex(li => {
      const link = li.querySelector('a')
      return link && eqPaths(link.getAttribute('href'), window.location.pathname)
   })
   if (n_ !== -1) {
      listItems[n_].classList.add('active-page')
   }
}

function loadSubHeader () {
   fetch('/shared/sub-header.html')
      .then(response => response.text())
      .then(data => {
         const header = document.createElement('div')
         header.innerHTML = data
         activateCurrentLink(header)
         const divElements = header.children
         const grid = document.getElementById('grid')
         for (let i = Math.min(3, divElements.length - 1); i >= 0; --i) {
            grid.insertBefore(divElements[i], grid.firstChild)
         }
      })
      .catch(error => console.error('Error loading shared HTML:', error))
}

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
