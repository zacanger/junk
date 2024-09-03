const React = require('react')

const PersonList = React.createClass({
  propTypes: {
    onCardClick: React.PropTypes.func,
    people: React.PropTypes.array
  },
  getDefaultProps(){
    return {
      onCardClick: () => {},
      people: []
    }
  },
  render (){
    return (
      <div>
        {this.props.people.map(person => {
          return (
            <div key={person.name} style={styles.card} onClick={this.props.onCardClick.bind(null, person.name)} >
              {person.name}
            </div>
          )
        })}
      </div>
    )
  }
})

const styles = {
  card: {
    padding: '10px',
    background: '#f5f5f5',
    borderRadius: '3px',
    border: '1px solid #e5e5e5',
    margin: '20px',
    fontFamily: 'Helvetica'
  }
}

module.exports = PersonList
