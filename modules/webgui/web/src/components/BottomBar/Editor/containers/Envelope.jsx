import React, {Component} from 'react'
import PropTypes from 'prop-types';
import Draggable from 'react-draggable'
import { connect } from 'react-redux';
import EnvelopeCanvas from '../presentational/EnvelopeCanvas'
import { toggleActiveEnvelope, toggleActivePoint, movePoint} from '../actions';

class Envelope extends Component {
  constructor(props) {
    super(props);
    this.state = {
      clickable :[true, true, true, true]
    }
    this.handleDrag = this.handleDrag.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  handleDrag(e, ui, id) {
    this.state.clickable[id]= false;
    let position = {
        x: this.props.points[id].position.x + ui.deltaX,
        y: this.props.points[id].position.y,
      }

    if(!this.props.points[id].anchor)
      position.y = position.y + ui.deltaY;
    this.props.MovePoint(position, id, this.props.id);
  }

  handleClick(pointId) {
    if (this.state.clickable[pointId] === false) {
      this.state.clickable[pointId]= true;
    }
    else {
      const {active, id} = this.props;
      if (active === true) {
        this.props.TogglePoint(id, pointId);
      }
      else if(this.hasActiveChild()) {
        this.props.TogglePoint(id, pointId);
      }
      else {
        this.props.ToggleEnvelope(id);
      }
    }
  }

  hasActiveChild() {
    var hasActiveChild = false;
    this.props.points.forEach(function(point) {
      if (point.active)
        hasActiveChild = true;
    })
    return hasActiveChild;
  }

  render() {
    const { points, height, width, active } = this.props;
    return (
      <EnvelopeCanvas
        handleClick={(pointId) => this.handleClick(pointId)}
        handleDrag={(e, ui, pointId) => this.handleDrag(e, ui, pointId)}
        points={points}
        height={height}
        width={width}
        active={active}
      />
    );
  }
}
Envelope.propTypes = {
  points: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y: PropTypes.number.isRequired,
            }).isRequired,
      anchor: PropTypes.bool.isRequired,
      color: PropTypes.string.isRequired,
    }).isRequired,
  ).isRequired,
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
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
    MovePoint: (position, id, envelopeId) => {
      dispatch(movePoint(id, envelopeId, position));
    },
  }
}

Envelope = connect(
  null,
  mapDispatchToProps,
  )(Envelope)

export default Envelope;
