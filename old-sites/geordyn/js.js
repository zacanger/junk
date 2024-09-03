// SENTENCES
var sections = [
  {	sentence : ' a multitalented developer based in the US' }
, { sentence : ' back at it again' }
, { sentence : ' one of my favourite people, let\'s be honest' }
, {	sentence : ' trying to make you laugh' }
, { sentence : ' making you laugh.' }
, {	sentence : ' always giving a shit' }
, {	sentence : ' finally updating her portfolio' }
, {	sentence : ' all in, or nothing' }
, {	sentence : ' missing people once in a while' }
, {	sentence : ' complaining about stuff' }
, { sentence : ' lovely.' }
, {	sentence : ' not sure if this is going too far' }
, {	sentence : ' curious to know you' }
, { sentence : ' making things she loves' }
, {	sentence : ' missing her alarm again' }
, { sentence : ' educating people' }
, { sentence : ' educating herself.' }
, { sentence : ' the best.' }
]

var i              = 0
  , j              = 0
  , k              = 0
  , forward        = true
  , opening        = false
  , interval       = 80
  , beginning      = 'Geordyn Ader is'
  , lengthArray    = sections.length
  , currentPart    = ''
  , lengthSentence = 0

// TYPING
function writing (text) {
  lengthSentence = sections[i].sentence.length
  var body = $('body')
  if (!opening) { // first part
    setTimeout(function () {
      if (k < beginning.length) {
        if (beginning[k] === '<') {
          currentPart += ' <br id="brName">'
          k = k + 4
        }
        currentPart += beginning[k]
        text.html(currentPart)
        k++
        writing(text)
      } else if (k === (beginning.length)) {
        currentPart += ' <br>'
        text.html(currentPart)
        opening = true
        writing(text)
      }
    }, interval)
  } else if (opening) { // sentences
    setTimeout(function () {
      interval = 80
      if (j === lengthSentence) {
        forward = false
      }
      if (j === lengthSentence - 2) {
        $('.afterTyping').one().addClass('onScreen')
      }
      if (j === lengthSentence - 1 && forward) {
        interval = 2000
      }
      if (j < lengthSentence && forward) {
        if (sections[i].sentence[j] === '&') {
          currentPart += '<strong>'
        } else if (sections[i].sentence[j] === '%') {
          currentPart += '</strong>'
        } else {
          currentPart += sections[i].sentence[j]
        }
        text.html(currentPart)
        j++
      } else if (j > 0 && !forward) {
        if (sections[i].sentence[j] === '&') {
          currentPart = currentPart.slice(0, - 8)
        } else if (sections[i].sentence[j] === '%') {
          currentPart = currentPart.slice(0, - 9)
        } else {
          currentPart = currentPart.slice(0, - 1)
        }
        text.html(currentPart)
        j--
      } else if (j === 0) {
        forward = true
        i++ // loops between sections
      }
      if (i === lengthArray) {
        i = 0
      }
      writing(text)
    }, interval)
  }
}

// BACKGROUND LOOP
function rand (min, max) {
  return min + Math.random() * (max - min)
}
function changebackground () {
  var body = $('body')
    , h    = rand(1, 360)
    , s    = rand(80, 90)
    , l    = rand(50, 60)
    , h2
  if (h < 180) {
    h2 = h + 180
  } else {
    h2 = h - 180
  }
  body.css({ // looping background
    'background': 'hsl(' + h + ',' + s + '%,' + l + '%)'
  })
  $('.fixedBg').css({ // background on hover
    'background': 'hsl(' + h + ',' + s + '%,' + l + '%)',
    'color': 'hsl(' + h2 + ',' + s + '%,' + l + '%)'
  })
  $('.coloredHover').css({ // color links on hover
    'color': 'hsl(' + h + ',' + s + '%,' + l + '%)'
  })

}

// COLORS LOOP - OLD/LINKS COLOR
// that was the original comment... idk.
function loopColors () {
  var selector = $('.loopCol')
    , h        = rand(1, 360)
    , s        = rand(0, 100)
    , l        = rand(0, 80)
  selector.css({
    'color': 'hsl(' + h + ',' + s + '%,' + l + '%)'
  })
}

// NOOB SHIT
// also a comment from the original code, lolwut
$(document).ready(function () {

// BACKGROUND
  changebackground()
  setTimeout(function () {
    $('body').removeClass('noTransition')
    $('fixedBg').removeClass('noTransition')
    changebackground()
  }, 2000)
  setInterval(function () {
    changebackground()
  }, 20000)

// TYPING
  var firstTimer = 5000
    , text       = $('.jstext')
  setTimeout(function () {
    writing(text)
  }, firstTimer)

// HOVER
  if (navigator.userAgent.toLowerCase().indexOf('firefox') > -1) {
    $('body').addClass('firefoxFix')
  }

})

// TWEENMAX FUN
$(document).ready(function ($) {
  var bgFixed      = $('.fixedBg')
    , elements     = $('.fixedBg span')
    , triggerHover = $('.loopCol')
  tlHoverIn  = new TimelineMax()
  tlHoverOut = new TimelineMax()

  triggerHover.hover(

    function () {
      TweenMax.to($(this).next('.fixedBg'), 0.5, {autoAlpha: 1})
      TweenMax.staggerTo($(this).next('.fixedBg').find('span'), 0.8, { y: 0, ease: Expo.easeOut}, 0)
    },

    function () {
      TweenMax.to($(this).next('.fixedBg'), 0.5, {autoAlpha: 0})
      TweenMax.to($(this).next('.fixedBg').find('span').eq(0), 0.8, { y: 30, ease: Expo.easeOut})
      TweenMax.to($(this).next('.fixedBg').find('span').eq(1), 0.8, { y: 60, ease: Expo.easeOut})
      TweenMax.to($(this).next('.fixedBg').find('span').eq(2), 0.8, { y: 90, ease: Expo.easeOut})
      TweenMax.to($(this).next('.fixedBg').find('span').eq(3), 0.8, { y: 120, ease: Expo.easeOut})
    }
  )
})

