module.exports = (paginationInfo, pages) => `
  <p>
    <strong>${paginationInfo.total_results}</strong> &mdash; <strong>${
  paginationInfo.total_pages
}</strong>
  </p>
  <div>
    ${
      paginationInfo.has_previous_page
        ? `<a href="/list/${paginationInfo.previous_page}">prev</a>`
        : ''
    }

    ${pages
      .map(
        (page) => `
      <a href="/list/page">${page}</a>
    `
      )
      .join('')}

    ${
      paginationInfo.has_next_page
        ? `<a href="/list/${paginationInfo.next_page}">next</a>`
        : ''
    }
  </div>
`
