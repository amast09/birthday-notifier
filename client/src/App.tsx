import React from "react";

const CLIENT_ID = process.env.REACT_APP_GOOGLE_API_CLIENT_ID;
const REDIRECT_URI = process.env.REACT_APP_GOOGLE_OAUTH_REDIRECT_URI;
// Verify these are the smallest set of scopes required
const SCOPES = "https://www.googleapis.com/auth/contacts.readonly https://www.googleapis.com/auth/userinfo.email email";
const OAUTH_URL = `https://accounts.google.com/o/oauth2/v2/auth?client_id=${CLIENT_ID}&scope=${SCOPES}&access_type=offline&response_type=code&redirect_uri=${REDIRECT_URI}`;

function App() {
  return (
    <div className="App">
      <a href={OAUTH_URL} target="_blank" rel="noreferrer">
        Get Birthday Notifications
      </a>
    </div>
  );
}

export default App;
