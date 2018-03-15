import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../common/Icon/Icon';

const MarkerInfo = (props) => {
  const { position, size } = props;

  return (
    <div style={{ position: 'absolute', left: `${position[0]}px`, bottom: `${position[1]}px`, textAlign: 'center' }}>
      <Icon icon="info_outline" style={{ fontSize: `${size}em` }} />
      <p style={{ fontSize: `${size / 2}em` }}>{props.name}</p>
    </div>
  );
};

MarkerInfo.propTypes = {
  position: PropTypes.arrayOf(PropTypes.string).isRequired,
  name: PropTypes.string.isRequired,
  size: PropTypes.number.isRequired,
};

export default MarkerInfo;
