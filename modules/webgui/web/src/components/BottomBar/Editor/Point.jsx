import React, {Component} from 'react'
import Draggable from 'react-draggable'
import { connect } from 'react-redux';
import { toggleActiveEnvelope, toggleActivePoint} from './actions';
import PropTypes from 'prop-types';
import styles from './Point.scss';

class Point extends Component{
  constructor(props) {
    super(props);

    this.state = {
      alteredPosition: {
        x: this.props.position.x,
        y: this.props.position.y,
      },
      clickable: true,
    }
    this.handleDrag = this.handleDrag.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  handleDrag(e, ui) {
    const {alteredPosition, actualPosition} = this.state;
    this.setState({
      clickable: false,
    });

    let position = {
        x: this.props.position.x + ui.deltaX,
        y: this.props.position.y,
      }

    if(!this.props.anchor)
      position.y = position.y + ui.deltaY;
    this.props.handleOnDrag(position, this.props.id, this.props.envelope.id);
  }

  hasActiveSibling(envelope) {
    var hasActiveSibling = false;
    envelope.points.forEach(function(point) {
      if (point.active)
        hasActiveSibling = true;
    })
    return hasActiveSibling;
  }

  handleClick() {
    if (this.state.clickable === false) {
      this.setState({
        clickable: true
      })
    }
    else {
      const {id, active, envelope} = this.props;
      if (envelope.active === true) {
        this.props.TogglePoint(this.props.envelope.id, this.props.id);
      }
      else if(this.hasActiveSibling(envelope)) {
        this.props.TogglePoint(this.props.envelope.id, this.props.id);
      }
      else {
        this.props.ToggleEnvelope(this.props.envelope.id);
      }
    }
  }

  render() {
    const { height, width, color, active, anchor, envelope} = this.props;
    const {x, y} = this.state.alteredPosition;
    return (
      <Draggable defaultPosition={this.state.alteredPosition} onDrag={this.handleDrag} axis={anchor ? "x" : "both"} bounds={{top: 0, left: 0, right: width, bottom: (height - 10)}}>
        <svg className={(active || envelope.active ) ? styles.Active : styles.Point} width={20} height={20} onClick={this.handleClick}>
          <circle cx={10} cy={10} r={(active || envelope.active ) ? 8 : 10} fill={color} />
        </svg>
      </Draggable>
    );
  }
}
Point.propTypes = {
  handleOnDrag: PropTypes.func.isRequired,
  position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
  anchor: PropTypes.bool.isRequired,
  color: PropTypes.string.isRequired,
  id: PropTypes.number.isRequired,
  active: PropTypes.bool.isRequired,
}

const mapDispatchToProps = (dispatch) => {
  return {
    ToggleEnvelope: (id) => {
      dispatch(toggleActiveEnvelope(id));
    },
    TogglePoint: (envelopeId, pointId) => {
      dispatch(toggleActivePoint(envelopeId, pointId));
    },

  }
}

Point = connect(
  null,
  mapDispatchToProps,
  )(Point)

export default Point;
