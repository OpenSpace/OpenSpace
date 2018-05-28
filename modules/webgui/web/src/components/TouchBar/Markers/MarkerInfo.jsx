import React from 'react';
import PropTypes from 'prop-types';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './MarkerInfo.scss';
import MarkerInfoIcon from './MarkerInfoIcon';

const MarkerInfo = (props) => {
  const { position, size, showInfo, identifier, showLabel, planetRadius, planetInfo } = props;

  const positionStyles = {
    MarkerInfo: {
      left: `${position[0]}px`,
      bottom: `calc(${position[1]}px + ${planetRadius}px)`,
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
      {showInfo &&
        <MarkerInfoIcon
          identifier={identifier}
          positionStyles={positionStyles}
          planetInfo={planetInfo}
        />}
      {showLabel &&
        <SmallLabel
          style={positionStyles.Text}
        >
          {identifier}
        </SmallLabel>}
    </div>
  );
};

MarkerInfo.propTypes = {
  position: PropTypes.arrayOf(PropTypes.string),
  identifier: PropTypes.string.isRequired,
  size: PropTypes.number.isRequired,
  showInfo: PropTypes.bool,
  planetInfo: PropTypes.objectOf(PropTypes.string),
  showLabel: PropTypes.bool.isRequired,
  planetRadius: PropTypes.number.isRequired,
};

MarkerInfo.defaultProps = {
  position: '',
  showInfo: false,
  planetInfo: {},
};

export default MarkerInfo;
