var React     = require('react')
  , ReactDOM  = require('react-dom')

var App = React.createClass({
  getInitialState() {
    return {
      textToDisplay: 'foo, bar'
    }
  },

  componentdidmount() {
    return {
      setTimeout(() => {
        this.setState({  textToDisplay: 'qux, bax'  })
      }, 500)
    }
  },

  componentWillMount() {
    return {}
  },

  _handleClick() {
    this.setState({textToDisplay: 'yeah. good job.'})
  }

  render() {
    return (
      <span onCLick={this._handleClick} className="oi">{this.state.textToDisplay}</span>
      <input type='text' value='' placeholder='howdy' />
    )
  }
})

ReactDOM.render(<App />, document.getElementById('app'))
