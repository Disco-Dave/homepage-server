import '@fortawesome/fontawesome-free/js/solid'
import '@fortawesome/fontawesome-free/js/fontawesome'

const searchForm = document.getElementById('search-form');
const searchInput = searchForm.querySelector('input');
const tabs = document.querySelectorAll('.tab');

function getUrl(type) {
  switch (type) {
    case 'Arch Wiki':
      return 'https://wiki.archlinux.org/index.php?search=';

    case 'Hoogle':
      return 'https://hoogle.haskell.org/?hoogle=';

    case 'Wikipedia':
      return 'https://en.wikipedia.org/w/index.php?search=';

    case 'Youtube':
      return 'https://www.youtube.com/results?search_query=';

    case 'Mozilla Web Docs':
      return 'https://developer.mozilla.org/en-US/search?q=';

    default:
      return 'https://duckduckgo.com/?q='
  }
}

function search(type, value) {
  const encodedValue = encodeURIComponent(value);
  document.location = getUrl(type) + encodedValue;
}


searchForm.addEventListener('submit', (e) => {
  e?.preventDefault?.();

  const activeTab = document.querySelector('.tab--active');
  const type = activeTab.attributes.title.value;
  const value = searchInput.value;
  search(type, value);
});

const setTab = (tab) => (e) => {
  e?.preventDefault?.();

  for (const otherTab of tabs) {
    otherTab.classList.remove('tab--active');
  }

  searchInput.placeholder = 'Search ' + tab.attributes.title.value;
  tab.classList.add('tab--active');
}

for (const tab of tabs) {
  const button = tab.querySelector('button');
  button.addEventListener('click', setTab(tab));
}

