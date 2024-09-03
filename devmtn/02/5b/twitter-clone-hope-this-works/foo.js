$('.content').click(function(){
  var t = true

  if (t) {
    $(this).find('.stats').toggle(t)
    $(this).find('.reply').toggle(t)
    swap = false
  } else {
    $(this).find('.stats').toggle(t)
    $(this).find('.reply').toggle(t)
    swap = false
  }
})

