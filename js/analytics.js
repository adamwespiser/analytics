

const API_KEY = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789';


fetchSessionId = async () => {
  var settings = {
      method: 'GET',
      headers: {
          Accept: 'application/json',
          'Content-Type': 'application/json'
      }
  };
  try {
      const fetchResponse = await fetch(`http://localhost:8080/session/?auth=${API_KEY}`, settings);
      return await fetchResponse.json();
  } catch (e) {
      return e;
  }
};

fetchSessionId();
