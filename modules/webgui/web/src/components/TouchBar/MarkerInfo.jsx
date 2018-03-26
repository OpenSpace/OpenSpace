import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../common/Icon/Icon';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import styles from './MarkerInfo.scss';

const MarkerInfo = (props) => {
  const { position, size } = props;

  const positionStyles = {
    MarkerInfo: {
      left: `${position[0]}px`,
      bottom: `${position[1]}px`,
      marginBottom: `-${size}em`,
      marginLeft: `-${size}em`,
    },
    Icon: {
      fontSize: `${size}em`,
    },
    Text: {
      fontSize: `${size / 2}em`,
    },
  };

  return (
    <div className={styles.MarkerInfo} style={positionStyles.MarkerInfo}>
      <Icon className={`${styles.icon} ${props.showInfo && styles.active}`} icon="info_outline" style={positionStyles.Icon} />
      <SmallLabel style={positionStyles.Text}>{props.identifier}</SmallLabel>
    </div>
  );
};

MarkerInfo.propTypes = {
  position: PropTypes.arrayOf(PropTypes.string).isRequired,
  identifier: PropTypes.string.isRequired,
  size: PropTypes.number.isRequired,
  showInfo: PropTypes.bool.isRequired,
};

export default MarkerInfo;
