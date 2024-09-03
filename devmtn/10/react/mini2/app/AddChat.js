var React = require('react')
  , $ = require('jquery')

var AddChat = React.createClass({

  getDefaultProps: function(){
    return {
      url: 'https://api.parse.com/1/classes/chat'
    }
  },
  propTypes: {
    url: React.PropTypes.string.isRequired
  },
  addChat: function(){
    $.ajax({
      url: this.props.url
    , type: 'POST'
     , data: JSON.stringify({text: this.refs.newChatInput.getDOMNode().value})
    ,  beforeSend: function(request){
        request.setRequestHeader('X-Parse-Application-ID', '2k2uUtD7GBWgBTETII8WhYHDfZul3PvuFJ1fseDH')
        request.setRequestHeader('X-Parse-REST-API-Key', 'lxWtVLMvV2qGFpyKiexjqJlddP7DHfnmaVeROm3Q')
        request.setRequestHeader('Content-Type', 'application/json')
      },
      error: function(){
        console.log('error')
      },
      success: function(){
        this.refs.newChatInput.getDOMNode().value = ''
        console.log('success!')
      }.bind(this)
    })
  },
  handleSubmit: function(e){
    if (e.keyCode === 13){
      this.addChat()
    }
  },
  render: function(){
    return (
      <div className='form-group'>
        <input type='text' ref='newChatInput' placeholder='Compose Message'
               className='form-control' onKeyDown={this.handleSubmit} />
      </div>
    )
  }
});

module.exports = AddChat
