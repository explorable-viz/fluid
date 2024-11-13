function loadHeader () {
   fetch('/shared/header.html')
      .then(response => response.text())
      .then(data => {
         const header = document.createElement('div')
         header.innerHTML = data
         const divElement = header.children[0]
         const grid = document.getElementById('grid')
         grid.parentNode.insertBefore(divElement, grid)
      })
      .catch(error => console.error('Error loading shared HTML:', error))
}

function eqPaths(path1, path2) {
  return path1.replace(/\/+$/, '') === path2.replace(/\/+$/, '')
}

function loadSubHeader (n) {
   fetch('/shared/sub-header.html')
      .then(response => response.text())
      .then(data => {
         const header = document.createElement('div')
         header.innerHTML = data

         const listItems = header.querySelectorAll('li')

         const n_ = Array.from(listItems).findIndex(li => {
            const link = li.querySelector('a')
            return link && eqPaths(link.getAttribute('href'), window.location.pathname)
         })

         if (n_ !== -1) {
            listItems[n_].classList.add('active-page')
         }

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

   if (grid.classList.contains('data-pane-visible')) {
      grid.classList.remove('data-pane-visible');
   } else {
      grid.classList.add('data-pane-visible');
   }
}
