import React from 'react';
import Overlay from '../components/common/Overlay/Overlay';
import Error from '../components/common/Error/Error';

const NotFound = () => (
  <Overlay>
    <Error>
      <h1>404</h1>
      <p>
        Snap! Something went wrong here.
      </p>
    </Error>
  </Overlay>
);

export default NotFound;
