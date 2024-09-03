const layout = require('./layout')
const { name } = require('../config')

module.exports = () =>
  layout(`
    <div>
      <h1>${name}</h1>
      <a href="/list">Uploaded Files</a>
      <div style="margin-top:32px;">
        <form action="/upload" method="post" enctype="multipart/form-data">
          <div>
            <input type="file" id="ffile" name="file" autocomplete="off">
            <sdiv id="submit-row">
              <button type="submit">Submit</button>
              <span>Max file size: 50MB</span>
            </div>
          </div>
        </form>
      </div>
    </div>
  `)
