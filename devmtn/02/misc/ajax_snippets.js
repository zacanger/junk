$(document).ready(function(){
  var app = $('app')
  var header = $('<h3>Random Colour Palette</h3>')
  var button = $('<button>Colours!</button>')
  var clear = $('<button>New ones!</button>')
  var list = $('<ul></ul>')
  button.on('click', function(){
    $.ajax({
      method: 'GET',
      url:'http://www.colourlovers.com/api/palettes/random?format=json',
    }).then(data){
      var dataObj = JSON.parse(data)
      showData(dataObj)
    })
  }




  app.append(header)
  app.append(button)
  app.append(clear)
  app.append(palette)

  function showData(data){
    var image = indexOf('imageUrl')
    var paletteDiv = $('<div></div>')
  //    var image = $(image tag  + url)

      pallet.append(paletteDiv)
    }
  }

})
