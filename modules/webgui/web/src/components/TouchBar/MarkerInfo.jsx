import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../common/Icon/Icon';

const MarkerInfo = (props) => {
  const { position, size } = props;
  const styles = {
    MarkerInfo: {
      position: 'absolute',
      TextAlign: 'center',
      color: 'white',
      left: `${position[0]}px`,
      bottom: `${position[1]}px`,
      marginBottom: `-${size}em`,
      marginLeft: `-${size / 2}em`,
    },
    Icon: {
      fontSize: `${size}em`,
    },
    Text: {
      fontSize: `${size / 2}em`,
    },
  };

  return (
    <div style={styles.MarkerInfo}>
      <Icon icon="info_outline" style={styles.Icon} />
      <p style={styles.Text}>{props.identifier}</p>
    </div>
  );
};

MarkerInfo.propTypes = {
  position: PropTypes.arrayOf(PropTypes.string).isRequired,
  identifier: PropTypes.string.isRequired,
  size: PropTypes.number.isRequired,
};

export default MarkerInfo;
