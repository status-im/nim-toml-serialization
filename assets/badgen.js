// npm install badgen

const { badgen } = require('badgen')

// only `status` is required.
const svgString = badgen({
  label: 'TOML',      // <Text>
  labelColor: '555', // <Color RGB> or <Color Name> (default: '555')
  status: 'v1.0.0-rc.2',  // <Text>, required
  color: 'blue',     // <Color RGB> or <Color Name> (default: 'blue')
  style: 'classic',     // 'flat' or 'classic' (default: 'classic')
  icon: 'data:image/svg+xml;base64,...', // Use icon (default: undefined)
  iconWidth: 1,     // Set this if icon is not square (default: 13)
  scale: 1           // Set badge scale (default: 1)
})

fs = require('fs');
fs.writeFile("badge-TOML.svg", svgString, function (err) {
  if (err) return console.log(err);
  console.log('badge created');
});
