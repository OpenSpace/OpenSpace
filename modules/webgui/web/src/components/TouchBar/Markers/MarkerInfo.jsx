import React, {Component} from 'react';
import PropTypes from 'prop-types';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './MarkerInfo.scss';
import MakerInfoIcon from "./MakerInfoIcon";

class MarkerInfo extends Component {
  constructor(props) {
    super(props);
  }

render() {
  const { position, size, showInfo, identifier, showLabel } = this.props;

  const positionStyles = {
    MarkerInfo: {
      width: '200px',
      left: `${position[0]}px`,
      bottom: `${position[1]}px`,
      marginBottom: `-${size}em`,
      marginLeft: '-100px',
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
      <MakerInfoIcon
        identifier={identifier}
        positionStyles={positionStyles}
        planetInfo={this.props.planetInfo}/>}
      {showLabel &&
      <SmallLabel
        style={positionStyles.Text}>
        {identifier}
      </SmallLabel>}
    </div>
  );
}
}

MarkerInfo.propTypes = {
  position: PropTypes.arrayOf(PropTypes.string),
  identifier: PropTypes.string.isRequired,
  size: PropTypes.number.isRequired,
  showInfo: PropTypes.bool.isRequired,
  planetInfo: PropTypes.objectOf(PropTypes.string),
};

export default MarkerInfo;
