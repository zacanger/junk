var React = require('react')
  , ReactDOM = require('react-dom')
  , ReactRouter = require('react-router')
  , Router = ReactRouter.Router
  , Route = ReactRouter.Route
  , Link = ReactRouter.Link
  , IndexRoute = ReactRouter.IndexRoute
  , About = require('./About')
  , Contact = require('./Contact')
  , Home = require('./Home')

var App = React.createClass({
  render: function(){
    return (
      <div>
        <Link to='/'>Home</Link>
        <Link to='/about/z'>About</Link>
        <Link to='/contact'>Contact</Link>
        {this.props.children}
      </div>
    )
  }
})

ReactDOM.render(
  <Router>
    <Route component={App} path='/'>
      <IndexRoute component={Home} />
      <Route component={About} path='about/:name' />
      <Route component={Contact} path='contact' />
    </Route>
  </Router>,
  document.getElementById('app')
)
