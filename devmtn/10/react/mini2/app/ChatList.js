var React = require('react')
  , $ = require('jquery')

var ChatList = React.createClass({
  getInitialState: function(){
    return {
      chats: []
    }
  },
  propTypes: {
    url: React.PropTypes.string.isRequired
  },
  getDefaultProps: function(){
    return {
      url: 'https://api.parse.com/1/class/chat'
    }
  },
  getChats: function(){
    $.ajax({
      url: this.props.url
    , type: 'GET'
    , beforeSend: function(request){
        request.setRequestHeader('X-Parse-Application-ID', 'lxWtVLMvV2qGFpyKiexjqJlddP7DHfnmaVeROm3Q')
        request.setRequestHeader('X-Parse-REST-API-Key', 'eIUhXSoxR5KGNXTXBAkKttRSqGsZfF8o6qxwq6mY')
        request.setRequestHeader('Content-Type', 'application/json')
    },
    error: function(data){
      console.log('error')
    },
    success: function(data){
      if (this.isMounted()) {
        this.setState({
          chats: data.results
        })
      }
    }.bind(this)
    })
  },
  componentDidMount: function(){
    this.interval = setInterval(function(){
      this.getChats()
    }.bind(this), 1000)
  },
  componentWillUnmount: function(){
    clearInterval(this.interval)
  },
  render: function(){
    var list = this.state.chats.map(function(item, index){
      return <li className='list-group-item' key={item.objectId}>{item.next}</li>
    })
    return (
      <ul className='list-group'>{list}</ul>
    )
  }
})

module.exports = ChatList
