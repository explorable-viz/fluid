function loadSubHeader (n) {
   fetch('/sub-header.html')
      .then(response => response.text())
      .then(data => {
         const header = document.createElement('div')
         header.innerHTML = data
         const listItems = header.querySelectorAll('li')
         listItems[n].classList.add('active-page')
         const divElements = header.children
         const grid = document.getElementById('grid')
         for (let i = Math.min(3, divElements.length - 1); i >= 0; --i) {
            grid.insertBefore(divElements[i], grid.firstChild)
         }
      })
      .catch(error => console.error('Error loading shared HTML:', error))
}

function loadHeader () {
   fetch('/header.html')
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

function toggle(id) {
   const elem = document.getElementById(id)
   if (elem.style.visibility == 'hidden')
      elem.style.visibility = 'visible'
   else
      elem.style.visibility = 'hidden'
}
