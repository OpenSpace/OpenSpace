import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../common/Icon/Icon';

const MarkerInfo = (props) => {
  const { position } = props;

  return (
    <div style={{ position: 'absolute', left: `${position[0]}px`, bottom: `${position[1]}px` }}>
      <Icon icon="info_outline" className="extralarge" />
      <p>{props.name}</p>
    </div>
  );
};

MarkerInfo.propTypes = {
  position: PropTypes.arrayOf(PropTypes.string).isRequired,
  name: PropTypes.string.isRequired,
};

export default MarkerInfo;
