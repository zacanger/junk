$(document).ready(function () {

    $('.box').click(function () {           //If you click on anywthing with a class of box
        var color = getRandomColor();       //Invokes the get random color function and stores the return value to the color variable
        $(this).css('background', color);   //changes the color of 'this'

    });

    //This function below is a random color generator I found on Stack Overflow
    function getRandomColor() {
        var letters = '0123456789ABCDEF'.split('');
        var color = '#';
        for (var i = 0; i < 6; i++ ) {
            color += letters[Math.floor(Math.random() * 16)];
        }
        return color;
    }

//// Added jQuery UI ////
   $('.box').click(function() {    // click an element with 'box' class
       $(this).effect('bounce', {times:3}, 300);   // will bounce the box as it changes color
   });

////

    $('.box').hover(function() {

        originalBackground = $(this).css('background-color');
        //THIS VAR IS GLOBAL AND REALLY BAD PRACTICE BUT I WANT TO GO TO SLEEP. SORRY
        //Saves a ref to the original BG color.

        $(this).css('background', function() {
            return getRandomColor() 
            //Changes BG to random color on hover.
        })

        $(this).on('click', function() {
            originalBackground = getRandomColor();
            //Saves changes to BG color after mouseleave
        })
    }, function() {
        $(this).css('background-color', originalBackground)
        //Reverts back to original color, or new color if clicked.
    });
});