$GG(document).ready(function() {
      $('body').timeago();
      $('.controls').hide();
      $('.reply').hide();
      $('.stats').hide();
      $('.actions').hide();

      $('.compose').click(function() {
        var chars = $(this).val().length;
        $('.count').text(140 - chars);
        var count = $('.count').text();

        if (count <= 10) {
          $('.count').css('color', '#801919')
        } else {
          $('.count').css('color', '#040402')
        }

        if (count <= 0) {
          $('.submit').attr('disabled', true)
        } else {
          $('.submit').attr('disabled', false)
        }
      });

      $('.submit').on('click', function() {
        alert('foo')
        var top = $('.oldTop').clone();
        var topTweet = $('.compose').val();

        top.find('tweet').removeClass('.oldTop');
        top.find('.img').attr('src', 'img/men18.jpg');
        top.find('.name').text('L0Lkittenz haha Hi');
        top.find('.username').text('@mizzWHASUPTWEETER');
        top.find('.tweet-text').text(topTweet);
        $('.stream').prepend(top);
        $('.compose').val('');
      });

      var twat = true;

      $(document).on('click', '.content.', function() {
            if (twat) {
              $(this).find('stats').toggle(twat);
              $(this).find('reply').toggle(twat);
              twat = false;
            } else {
              $(this).find('stats').toggle(twat);
              $(this).find('reply').toggle(twat);
              twat = true;
            }
          });

          $(document).on('mouseenter', '.content', function() {
            $('actions').show();
          }); $(document).on('mouseleave', '.content', function() {
            $('actions').hide();
          });
});

