var React = require('react')

var About = React.createClass({
  render: function(){
    return (
      <div>About: {this.props.params.name}</div>
    )
  }
})

module.exports = About
