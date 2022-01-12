const searchForm = document.getElementById('search-form');
const searchInput = searchForm.querySelector('input');
const tabs = document.querySelectorAll('.tab');

function search(href, value) {
  const encodedValue = encodeURIComponent(value);
  document.location = href + encodedValue;
}


searchForm.addEventListener('submit', (e) => {
  e?.preventDefault?.();

  const activeTab = document.querySelector('.tab--active');
  const href = activeTab.dataset.href;
  const value = searchInput.value;
  search(href, value);
});

const setTab = (tab) => (e) => {
  e?.preventDefault?.();

  for (const otherTab of tabs) {
    otherTab.classList.remove('tab--active');
  }

  searchInput.placeholder = 'Search ' + tab.attributes.title.value;
  tab.classList.add('tab--active');

  searchInput.focus();
}

for (const tab of tabs) {
  const button = tab.querySelector('button');
  button.addEventListener('click', setTab(tab));
}

