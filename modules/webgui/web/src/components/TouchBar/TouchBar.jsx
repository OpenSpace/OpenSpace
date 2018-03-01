import React from 'react';
import FocusMenu from './FocusMenu/FocusMenu';
import Markers from './Markers';

const TouchBar = () => (
  <div className={'TouchBar'}>
    <FocusMenu />
    <Markers />
  </div>
);

export default TouchBar;
