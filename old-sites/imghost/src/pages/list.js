const layout = require('./layout')
const pagination = require('./pagination')
const filesize = require('filesize')
const dateformat = require('dateformat')

module.exports = ({ paginationInfo, pages, files }) =>
  layout(`
    <div>
      <div>
        ${pagination(paginationInfo, pages)}
      </div>
      <div>
        <table>
          <tr>
            <th>thumb</th>
            <th>size</th>
            <th>modified</th>
          </tr>
          ${files
            .map(
              (file) => `
            <tr>
              <td>
                <a href="/${file.name}" target="_blank">
                  <img style="max-height: 150px; max-width: 150px;" src="/${
                    file.name
                  }">
                </a>
              </td>
              <td>${filesize(file.size)}</td>
              <td>${dateformat(file.mtime)}</td>
            </tr>
          `
            )
            .join('')}
          </table>
        </div>
      </div>
      <div>
        ${pagination(paginationInfo, pages)}
      </div>
    </div>
  `)
