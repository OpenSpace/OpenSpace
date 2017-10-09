import React, {Component} from 'react'
import PropTypes from 'prop-types';
import Draggable from 'react-draggable'

import styles from './Envelope.scss'

class Circle extends Component {
  constructor(props) {
    super(props);

  }

  render() {
    const { position } = this.props;
    return (
      <Draggable defaultPosition={position}>
        <circle className={styles.Circle} onClick={this.handleOnclick} r={10} fill={this.props.color} />
      </Draggable>
      );
  }
}
Circle.propTypes = {
  position: PropTypes.shape({
    x: PropTypes.number,
    y: PropTypes.number,
  }),
};

Circle.defaultProps = {
  position: { x: 10, y: 10 },
};

class Envelope extends Component {
    constructor(props) {
    super(props);

    this.state = {
        circles: [],
    }
    this.onClick = this.handleClickEvent.bind(this);
  }

  handleClickEvent(e) {
    this.setState({ lastX: e.nativeEvent.offsetX, lastY: e.nativeEvent.offsetY });
    this.state.circles.push(<Circle {...this.props} position={{ x: e.nativeEvent.offsetX, y: e.nativeEvent.offsetY}}/>);
  }

  render() {
    return (
      <div className={styles.Envelope} onClick={this.onClick}>
        {this.state.circles}
      </div>
      );
  }
}
export default Envelope;
