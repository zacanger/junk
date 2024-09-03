const React = require('react')
const ReactDOM = require('react-dom')
const $ = require('jquery')
const PersonList = require('./PersonList')

const App = React.createClass({
  getInitialState(){
    return {
      people: []
    }
  },
  componentDidMount(){
    $.get('http://swapi.co/api/people', response => {
      console.log(data)
    })
  },
  _handleCardClick(name, e){
    alert(name)
  },
  render (){
    return (
      <PersonList people={this.state.people} onCardClick={this._handleCardClick} />
    )
  }
})

ReactDOM.render(<App />, document.getElementById('app'))
